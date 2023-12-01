import std.algorithm, std.stdio, std.string, std.ascii;
// Count words in a file using ranges.
void main()
{
    auto file = File("input1.txt"); // Open for reading
    auto sum = 0;
    foreach (line; file.byLine()) {
        auto first = -1;
        auto last = 0;
        foreach (chr; line) {
            if(first == -1) {
                if(isDigit(chr)) {
                    first = chr - 48;
                    last = chr - 48;
                }
            } else if (isDigit(chr)) {
                last = chr - 48;
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
