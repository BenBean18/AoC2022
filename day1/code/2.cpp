#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <regex>
#include <vector>

std::vector<std::string> getLinesOfFile(char *filename, char delim = '\n') {
    std::vector<std::string> v;
    std::ifstream f(filename);
    std::string l;
    while (getline(f, l, delim)) {
        v.push_back(l);
    }
    return v;
}

std::vector<int> getIntsOfFile(char *filename) {
    std::vector<int> v;
    std::ifstream f(filename);
    int i;
    std::string l;
    while (getline(f, l)) {
        v.push_back(std::stoi(l));
    }
    return v;
}

std::string getFileContents(char* filename) {
    std::ifstream t(filename);
    std::stringstream buffer;
    buffer << t.rdbuf();
    return buffer.str();
}

int main(int argc, char** argv) {
    // Part 2: need the sum of the top 3 elves
    std::cout << "Reading " << argv[1] << " as puzzle input." << std::endl;
    // Get all lines of file
    std::vector<std::string> lines = getLinesOfFile(argv[1]);
    // Initialize a list of elves
    // Other way to do it: keep a running max, if this elf's total is greater than that, then make that the max
    // I'm going to do the list way for now and then try to optimize after I have a solution
    std::vector<int> elves; // index = elf #, value = elf calories
    // Initialize a count for the current elf
    int calories = 0;
    // For each line,
    for (std::string line : lines) {
        // if the line is not empty
        if (line != "") {
            // add the line as an integer to calories
            calories += std::stoi(line);
        } else {
            // if it is empty, add the elf & reset calories
            elves.push_back(calories);
            calories = 0;
        }
    }
    // Sort the elves
    std::sort(elves.begin(), elves.end());
    // get last elf index
    int lastElf = elves.size() - 1;
    // Initialize the sum of the three elves with the most calories
    int sumTop3 = 0;
    // Add the top 3
    for (int i = lastElf; i > lastElf - 3; i--) {
        sumTop3 += elves[i];
    }
    // Print out the solution!
    std::cout << "Solution is:" << std::endl << sumTop3 << std::endl;
}