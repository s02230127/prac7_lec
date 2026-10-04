#include <algorithm>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <fstream>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <unordered_map>
#include <vector>


constexpr std::uint32_t MAX_ID = std::numeric_limits<std::uint32_t>::max();

struct TEdge {
    std::uint32_t First;
    std::uint32_t Second;
    std::uint8_t Weight;
};

struct TArguments {
    bool Serialize = false;
    std::string Input;
    std::string Output;
};

TArguments ParseArguments(int argc, char* argv[]) {
    TArguments result;
    bool hasMode = false;
    for (int i = 1; i < argc; ++i) {
        const std::string argument = argv[i];
        if (argument == "-s" || argument == "-d") {
            if (hasMode) {
                throw std::runtime_error("Specify exactly one of -s and -d");
            }
            hasMode = true;
            result.Serialize = argument == "-s";
        } else if (argument == "-i" || argument == "-o") {
            if (i + 1 == argc) {
                throw std::runtime_error("Missing filename after " + argument);
            }
            std::string& path = argument == "-i" ? result.Input : result.Output;
            if (!path.empty()) {
                throw std::runtime_error("Repeated option: " + argument);
            }
            path = argv[++i];
        } else {
            throw std::runtime_error("Unknown option: " + argument);
        }
    }
    if (!hasMode || result.Input.empty() || result.Output.empty()) {
        throw std::runtime_error("Required options: -s or -d, -i FILE, -o FILE");
    }
    if (result.Input == result.Output) {
        throw std::runtime_error("Input and output filenames must differ");
    }
    return result;
}

std::uint32_t ParseNumber(std::string_view text, std::uint32_t maximum,
                          std::uint64_t lineNumber) {
    std::uint32_t value = 0;
    const auto parsed = std::from_chars(text.data(), text.data() + text.size(), value);
    if (text.empty() || parsed.ec != std::errc{} ||
        parsed.ptr != text.data() + text.size() || value > maximum)
    {
        throw std::runtime_error("Invalid number at line " + std::to_string(lineNumber));
    }
    return value;
}

std::vector<TEdge> ReadTsv(const std::string& path) {
    std::ifstream input(path, std::ios::binary);
    if (!input) {
        throw std::runtime_error("Cannot open input: " + path);
    }
    std::vector<TEdge> edges;
    std::string line;
    std::uint64_t lineNumber = 0;
    while (std::getline(input, line)) {
        ++lineNumber;
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        const std::string_view text(line);
        const std::size_t firstTab = text.find('\t');
        const std::size_t secondTab = firstTab == std::string_view::npos
            ? std::string_view::npos : text.find('\t', firstTab + 1);
        if (secondTab == std::string_view::npos ||
            text.find('\t', secondTab + 1) != std::string_view::npos)
        {
            throw std::runtime_error("Expected three tab-separated numbers at line " +
                                     std::to_string(lineNumber));
        }
        TEdge edge;
        edge.First = ParseNumber(text.substr(0, firstTab), MAX_ID, lineNumber);
        edge.Second = ParseNumber(text.substr(firstTab + 1, secondTab - firstTab - 1),
                                  MAX_ID, lineNumber);
        edge.Weight = static_cast<std::uint8_t>(
            ParseNumber(text.substr(secondTab + 1), 255, lineNumber));

        if (edge.First > edge.Second) {
            std::swap(edge.First, edge.Second);
        }
        edges.push_back(edge);
    }
    if (input.bad() || !input.eof()) {
        throw std::runtime_error("Cannot read input: " + path);
    }
    return edges;
}

void AppendInteger(std::vector<std::uint8_t>& data, std::uint64_t value) {
    do {
        std::uint8_t byte = static_cast<std::uint8_t>(value & 127);
        value >>= 7;
        if (value != 0) {
            byte |= 128;
        }
        data.push_back(byte);
    } while (value != 0);
}

std::uint64_t ReadInteger(const std::vector<std::uint8_t>& data, std::size_t& position) {
    std::uint64_t value = 0;
    for (unsigned shift = 0; shift < 64; shift += 7) {
        if (position == data.size()) {
            throw std::runtime_error("Truncated binary header");
        }
        const std::uint8_t byte = data[position++];
        if (shift == 63 && (byte & 254) != 0) {
            throw std::runtime_error("Integer overflow in binary header");
        }
        value |= static_cast<std::uint64_t>(byte & 127) << shift;
        if ((byte & 128) == 0) {
            return value;
        }
    }
    throw std::runtime_error("Invalid integer in binary header");
}

class TBitWriter {
public:
    explicit TBitWriter(std::vector<std::uint8_t>& data)
        : Data_(data)
    {
    }

    void WriteBits(std::uint32_t value, unsigned count) {
        while (count != 0) {
            const unsigned take = std::min(8 - Used_, count);
            const std::uint32_t mask = (1U << take) - 1;
            Pending_ |= static_cast<std::uint8_t>((value & mask) << Used_);
            value >>= take;
            Used_ += take;
            count -= take;
            if (Used_ == 8) {
                Data_.push_back(Pending_);
                Pending_ = 0;
                Used_ = 0;
            }
        }
    }

    void WriteMostSignificantBits(std::uint32_t value, unsigned count) {
        if (count == 0) {
            return;
        }
        value = ((value >> 1) & 0x55555555U) | ((value & 0x55555555U) << 1);
        value = ((value >> 2) & 0x33333333U) | ((value & 0x33333333U) << 2);
        value = ((value >> 4) & 0x0F0F0F0FU) | ((value & 0x0F0F0F0FU) << 4);
        value = ((value >> 8) & 0x00FF00FFU) | ((value & 0x00FF00FFU) << 8);
        value = (value >> 16) | (value << 16);
        WriteBits(value >> (32 - count), count);
    }

    void WriteGolomb(std::uint32_t value, std::uint32_t modulus) {
        std::uint32_t quotient = value / modulus;
        while (quotient >= 32) {
            WriteBits(0, 32);
            quotient -= 32;
        }
        WriteBits(0, quotient);
        WriteBits(1, 1);

        if (modulus == 1) {
            return;
        }

        unsigned bits = 0;
        std::uint32_t power = 1;
        while (power < modulus) {
            power <<= 1;
            ++bits;
        }
        const std::uint32_t cutoff = power - modulus;
        const std::uint32_t remainder = value % modulus;
        if (remainder < cutoff) {
            WriteMostSignificantBits(remainder, bits - 1);
        } else {
            WriteMostSignificantBits(remainder + cutoff, bits);
        }
    }

    void WriteRice(std::uint32_t value, unsigned parameter) {
        if (parameter == 32) {
            WriteBits(value, 32);
            return;
        }
        std::uint32_t quotient = value >> parameter;
        while (quotient >= 32) {
            WriteBits(0, 32);
            quotient -= 32;
        }
        WriteBits(0, quotient);
        WriteBits(1, 1);
        WriteBits(value, parameter);
    }

    void Finish() {
        if (Used_ != 0) {
            Data_.push_back(Pending_);
        }
    }

private:
    std::vector<std::uint8_t>& Data_;
    std::uint8_t Pending_ = 0;
    unsigned Used_ = 0;
};

class TBitReader {
public:
    TBitReader(const std::vector<std::uint8_t>& data, std::size_t position)
        : Data_(data)
        , Position_(position)
    {
    }

    std::uint32_t ReadBits(unsigned count) {
        std::uint32_t value = 0;
        unsigned written = 0;
        while (count != 0) {
            if (Position_ == Data_.size()) {
                throw std::runtime_error("Truncated binary data");
            }
            const unsigned take = std::min(8 - Used_, count);
            const std::uint32_t part = (Data_[Position_] >> Used_) & ((1U << take) - 1);
            value |= part << written;
            written += take;
            Used_ += take;
            count -= take;
            if (Used_ == 8) {
                ++Position_;
                Used_ = 0;
            }
        }
        return value;
    }

    std::uint32_t ReadMostSignificantBits(unsigned count) {
        if (count == 0) {
            return 0;
        }
        std::uint32_t value = ReadBits(count);
        value = ((value >> 1) & 0x55555555U) | ((value & 0x55555555U) << 1);
        value = ((value >> 2) & 0x33333333U) | ((value & 0x33333333U) << 2);
        value = ((value >> 4) & 0x0F0F0F0FU) | ((value & 0x0F0F0F0FU) << 4);
        value = ((value >> 8) & 0x00FF00FFU) | ((value & 0x00FF00FFU) << 8);
        value = (value >> 16) | (value << 16);
        return value >> (32 - count);
    }

    std::uint32_t ReadGolomb(std::uint32_t modulus, std::uint32_t maximum) {
        std::uint32_t quotient = 0;
        while (ReadBits(1) == 0) {
            if (quotient == maximum / modulus) {
                throw std::runtime_error("Number exceeds its range in binary data");
            }
            ++quotient;
        }

        std::uint32_t remainder = 0;
        if (modulus != 1) {
            unsigned bits = 0;
            std::uint32_t power = 1;
            while (power < modulus) {
                power <<= 1;
                ++bits;
            }
            const std::uint32_t cutoff = power - modulus;
            remainder = ReadMostSignificantBits(bits - 1);
            if (remainder >= cutoff) {
                remainder = (remainder << 1) | ReadMostSignificantBits(1);
                remainder -= cutoff;
            }
        }

        const std::uint64_t value = static_cast<std::uint64_t>(quotient) * modulus + remainder;
        if (value > maximum) {
            throw std::runtime_error("Number exceeds its range in binary data");
        }
        return static_cast<std::uint32_t>(value);
    }

    std::uint32_t ReadRice(unsigned parameter, std::uint32_t maximum) {
        std::uint64_t value;
        if (parameter == 32) {
            value = ReadBits(32);
        } else {
            std::uint32_t quotient = 0;
            while (ReadBits(1) == 0) {
                if (quotient == (maximum >> parameter)) {
                    throw std::runtime_error("Number exceeds its range in binary data");
                }
                ++quotient;
            }
            value = (static_cast<std::uint64_t>(quotient) << parameter) |
                    ReadBits(parameter);
        }
        if (value > maximum) {
            throw std::runtime_error("Number exceeds its range in binary data");
        }
        return static_cast<std::uint32_t>(value);
    }

    void Finish() {
        if (Used_ != 0) {
            if ((Data_[Position_] >> Used_) != 0) {
                throw std::runtime_error("Invalid padding in binary data");
            }
            ++Position_;
        }
        if (Position_ != Data_.size()) {
            throw std::runtime_error("Extra bytes after binary data");
        }
    }

private:
    const std::vector<std::uint8_t>& Data_;
    std::size_t Position_;
    unsigned Used_ = 0;
};

unsigned ChooseRiceParameter(const std::vector<std::uint32_t>& values) {
    unsigned bestParameter = 32;
    std::uint64_t bestSize = static_cast<std::uint64_t>(values.size()) * 32;
    for (unsigned parameter = 0; parameter < 32; ++parameter) {
        std::uint64_t size = 0;
        for (std::uint32_t value : values) {
            size += static_cast<std::uint64_t>(value >> parameter) + 1 + parameter;
        }
        if (size < bestSize) {
            bestSize = size;
            bestParameter = parameter;
        }
    }
    return values.empty() ? 0 : bestParameter;
}

std::uint32_t NeighborGolombModulus(std::uint64_t available, std::uint64_t count) {
    if (count == 0) {
        return 1;
    }

    const std::uint64_t numerator = (available + 1) * 45426;
    const std::uint64_t denominator = (count + 1) * 65536;
    const std::uint64_t modulus = (numerator + denominator / 2) / denominator;
    return static_cast<std::uint32_t>(std::max<std::uint64_t>(1, modulus));
}

std::vector<std::uint32_t> CollectIds(const std::vector<TEdge>& edges) {
    std::vector<std::uint32_t> ids;
    if (edges.size() > ids.max_size() / 2) {
        throw std::runtime_error("Graph is too large");
    }
    ids.reserve(edges.size() * 2);
    for (const TEdge& edge : edges) {
        ids.push_back(edge.First);
        ids.push_back(edge.Second);
    }
    std::sort(ids.begin(), ids.end());
    ids.erase(std::unique(ids.begin(), ids.end()), ids.end());
    if (ids.size() > MAX_ID) {
        throw std::runtime_error("Too many vertices");
    }
    return ids;
}

void ReplaceIdsWithIndices(std::vector<TEdge>& edges,
                           const std::vector<std::uint32_t>& ids) {
    std::unordered_map<std::uint32_t, std::uint32_t> indices;
    indices.max_load_factor(0.7F);
    indices.reserve(ids.size());
    for (std::size_t i = 0; i < ids.size(); ++i) {
        indices.emplace(ids[i], static_cast<std::uint32_t>(i));
    }
    for (TEdge& edge : edges) {
        edge.First = indices.at(edge.First);
        edge.Second = indices.at(edge.Second);
    }
}

std::vector<std::uint8_t> EncodeGraph(std::vector<TEdge> edges) {
    const std::vector<std::uint32_t> ids = CollectIds(edges);
    ReplaceIdsWithIndices(edges, ids);
    std::sort(edges.begin(), edges.end(), [](const TEdge& left, const TEdge& right) {
        if (left.First != right.First) {
            return left.First < right.First;
        }
        return left.Second < right.Second;
    });
    std::vector<std::uint32_t> counts(ids.size(), 0);
    for (std::size_t i = 0; i < edges.size(); ++i) {
        if (i != 0 && edges[i].First == edges[i - 1].First &&
            edges[i].Second == edges[i - 1].Second)
        {
            throw std::runtime_error("Parallel edges are not allowed");
        }
        ++counts[edges[i].First];
    }
    std::vector<std::uint32_t> gaps(ids.size());
    std::uint64_t nextId = 0;
    for (std::size_t i = 0; i < ids.size(); ++i) {
        gaps[i] = static_cast<std::uint32_t>(ids[i] - nextId);
        nextId = static_cast<std::uint64_t>(ids[i]) + 1;
    }
    const unsigned idParameter = ChooseRiceParameter(gaps);
    const unsigned countParameter = ChooseRiceParameter(counts);
    std::vector<std::uint8_t> data;
    AppendInteger(data, ids.size());
    AppendInteger(data, edges.size());
    data.push_back(static_cast<std::uint8_t>(idParameter));
    data.push_back(static_cast<std::uint8_t>(countParameter));
    TBitWriter writer(data);
    for (std::uint32_t gap : gaps) {
        writer.WriteRice(gap, idParameter);
    }
    std::size_t edgeIndex = 0;
    for (std::size_t vertex = 0; vertex < ids.size(); ++vertex) {
        const std::uint32_t count = counts[vertex];
        writer.WriteRice(count, countParameter);
        std::uint64_t nextNeighbor = vertex;
        for (std::uint32_t i = 0; i < count; ++i) {
            const std::uint32_t remainingCount = count - i;
            const std::uint32_t modulus = NeighborGolombModulus(
                ids.size() - nextNeighbor, remainingCount);
            const TEdge& edge = edges[edgeIndex++];
            writer.WriteGolomb(static_cast<std::uint32_t>(edge.Second - nextNeighbor), modulus);
            writer.WriteBits(edge.Weight, 8);
            nextNeighbor = static_cast<std::uint64_t>(edge.Second) + 1;
        }
    }
    writer.Finish();
    return data;
}

std::vector<TEdge> DecodeGraph(const std::vector<std::uint8_t>& data) {
    std::size_t position = 0;
    const std::uint64_t vertexCount = ReadInteger(data, position);
    const std::uint64_t edgeCount = ReadInteger(data, position);
    if (data.size() - position < 2) {
        throw std::runtime_error("Truncated binary header");
    }
    const unsigned idParameter = data[position++];
    const unsigned countParameter = data[position++];
    const std::uint64_t availableBits = static_cast<std::uint64_t>(data.size() - position) * 8;
    if (idParameter > 32 || countParameter > 32 || vertexCount > MAX_ID ||
        vertexCount > availableBits / 2 || edgeCount > availableBits / 9 ||
        vertexCount > edgeCount * 2 ||
        edgeCount > vertexCount * (vertexCount + 1) / 2)
    {
        throw std::runtime_error("Invalid graph sizes or coding parameters");
    }
    std::vector<std::uint32_t> ids(static_cast<std::size_t>(vertexCount));
    TBitReader reader(data, position);
    std::uint64_t nextId = 0;
    for (std::size_t i = 0; i < ids.size(); ++i) {
        const std::uint64_t maximumId = MAX_ID - (vertexCount - i - 1);
        const auto maximumGap = static_cast<std::uint32_t>(maximumId - nextId);
        ids[i] = static_cast<std::uint32_t>(nextId + reader.ReadRice(idParameter, maximumGap));
        nextId = static_cast<std::uint64_t>(ids[i]) + 1;
    }
    std::vector<TEdge> edges;
    if (edgeCount > edges.max_size()) {
        throw std::runtime_error("Graph is too large");
    }
    edges.reserve(static_cast<std::size_t>(edgeCount));
    for (std::size_t vertex = 0; vertex < ids.size(); ++vertex) {
        const auto maximumCount = static_cast<std::uint32_t>(
            std::min(vertexCount - vertex, edgeCount - edges.size()));
        const std::uint32_t count = reader.ReadRice(countParameter, maximumCount);
        std::uint64_t nextNeighbor = vertex;
        for (std::uint32_t i = 0; i < count; ++i) {
            const std::uint32_t remainingCount = count - i;
            const std::uint32_t modulus = NeighborGolombModulus(
                vertexCount - nextNeighbor, remainingCount);
            const auto maximumGap = static_cast<std::uint32_t>(
                vertexCount - remainingCount - nextNeighbor);
            const std::uint32_t gap = reader.ReadGolomb(modulus, maximumGap);
            const auto neighbor = static_cast<std::size_t>(nextNeighbor + gap);
            const auto weight = static_cast<std::uint8_t>(reader.ReadBits(8));
            edges.push_back({ids[vertex], ids[neighbor], weight});
            nextNeighbor = neighbor + 1;
        }
    }
    if (edges.size() != edgeCount) {
        throw std::runtime_error("Inconsistent edge count");
    }
    reader.Finish();
    return edges;
}

std::vector<std::uint8_t> ReadBinary(const std::string& path) {
    std::ifstream input(path, std::ios::binary | std::ios::ate);
    if (!input) {
        throw std::runtime_error("Cannot open input: " + path);
    }
    const std::streamoff size = input.tellg();
    std::vector<std::uint8_t> data;
    if (size < 0 || static_cast<std::uint64_t>(size) > data.max_size()) {
        throw std::runtime_error("Cannot determine input size: " + path);
    }
    data.resize(static_cast<std::size_t>(size));
    input.seekg(0);
    if (!data.empty()) {
        input.read(reinterpret_cast<char*>(data.data()), static_cast<std::streamsize>(data.size()));
    }
    if (!input) {
        throw std::runtime_error("Cannot read input: " + path);
    }
    return data;
}

void WriteBinary(const std::string& path, const std::vector<std::uint8_t>& data) {
    std::ofstream output(path, std::ios::binary);
    if (!output) {
        throw std::runtime_error("Cannot open output: " + path);
    }
    output.write(reinterpret_cast<const char*>(data.data()),
                 static_cast<std::streamsize>(data.size()));
    output.close();
    if (!output) {
        throw std::runtime_error("Cannot write output: " + path);
    }
}

void WriteTsv(const std::string& path, const std::vector<TEdge>& edges) {
    std::ofstream output(path, std::ios::binary);
    if (!output) {
        throw std::runtime_error("Cannot open output: " + path);
    }
    for (const TEdge& edge : edges) {
        output << edge.First << '\t' << edge.Second << '\t'
               << static_cast<unsigned>(edge.Weight) << '\n';
    }
    output.close();
    if (!output) {
        throw std::runtime_error("Cannot write output: " + path);
    }
}


int main(int argc, char* argv[]) {
    try {
        const TArguments arguments = ParseArguments(argc, argv);
        if (arguments.Serialize) {
            WriteBinary(arguments.Output, EncodeGraph(ReadTsv(arguments.Input)));
        } else {
            WriteTsv(arguments.Output, DecodeGraph(ReadBinary(arguments.Input)));
        }
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "Error: " << error.what() << '\n';
        return 1;
    }
}
