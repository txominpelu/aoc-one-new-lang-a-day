import std.algorithm, std.stdio, std.string, std.ascii;
// Count words in a file using ranges.
void main()
{
    auto file = File("input2.txt"); // Open for reading
    auto sum = 0;
    foreach (line; file.byLine()) {
        auto first = -1;
        auto last = 0;
        for( int i = 0; i < line.length; i = i + 1 ) {
            auto chr = line[i];
            if(first == -1) {
                if(isDigit(chr)) {
                    first = chr - 48;
                    last = chr - 48;
                }else if(isNumber(i, line)) {
                    first = convertNumber(i, line);
                    last = convertNumber(i, line);
                }
            } else if (isDigit(chr)) {
                last = chr - 48;
            } else if (isNumber(i, line)) {
                last = convertNumber(i, line);
            }
        }

        writeln("Line:");
        writeln("======================");
        writeln(first * 10 + last);
        sum = sum + first * 10 + last; 
    }
    writeln("Final sum:");
    writeln(sum);
}

bool isNumber(ulong i, char[] line) {
    auto numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];
    auto subline = line[i..$];
    foreach(number; numbers) {
        if(startsWith(subline, number)) {
            return true;
        }
    }
    return false;
}

int convertNumber(ulong i, char[] line) {
    auto numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];
    auto subline = line[i..$];
    for(int k = 0; k < numbers.length; k = k + 1) {
        auto number = numbers[k];
        if(startsWith(subline, number)) {
            return k+1;
        }
    }
    writeln("Failed end !!!!");
    return -1;
}
