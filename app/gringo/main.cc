// {{{ GPL License 

// This file is part of gringo - a grounder for logic programs.
// Copyright (C) 2013  Roland Kaminski

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// }}}

#include <gringo/input/nongroundparser.hh>
#include <gringo/input/programbuilder.hh>
#include <gringo/input/program.hh>
#include <gringo/ground/program.hh>
#include <gringo/logger.hh>
#include <iostream>
#include <stdexcept>

bool g_verbose = false;
#define LOG if (g_verbose) std::cerr

Gringo::Ground::Program prepare(Gringo::Output::OutputBase &out, std::vector<char const *> files) {
    using namespace Gringo;
    Input::Program prg;
    Defines defs;
    Input::NongroundProgramBuilder pb(prg, out, defs);
    Input::NonGroundParser parser(pb);
    for (auto &x : files) {
        LOG << "file: " << x << std::endl;
        parser.pushFile(x);
    }
    if (files.empty()) {
        LOG << "reading from stdin" << std::endl;
        parser.pushFile("-");
    }
    parser.parse();
    LOG << "************** parsed program **************" << std::endl << prg;
    LOG << "*********** extensional database ***********" << std::endl;
    prg.rewrite(defs);
    LOG << "************* rewritten program ************" << std::endl << prg;
    prg.check();
    if (message_printer()->hasError()) {
        throw std::runtime_error("grounding stopped because of errors");
    }
    return prg.toGround(out.domains);
}

void ground(Gringo::Output::OutputBase &out, std::vector<char const *> files) {
    using namespace Gringo;
    Ground::Program gPrg(prepare(out, files));
    LOG << "************* intermediate program *************" << std::endl << gPrg << std::endl;
    LOG << "************* grounded program *************" << std::endl;
    gPrg.linearize(out);
    gPrg.ground(out);
}

std::vector<std::string> split(std::string const &source, char const *delimiter = " ", bool keepEmpty = false) {
    std::vector<std::string> results;
    size_t prev = 0;
    size_t next = 0;
    while ((next = source.find_first_of(delimiter, prev)) != std::string::npos) {
        if (keepEmpty || (next - prev != 0)) { results.push_back(source.substr(prev, next - prev)); }
        prev = next + 1;
    }
    if (prev < source.size()) { results.push_back(source.substr(prev)); }
    return results;
}
std::pair<std::string, std::string> splitFirst(std::string const &source, char const *delimiter = " ") {
    size_t next = source.find_first_of(delimiter, 0);
    if (next == std::string::npos) { throw std::runtime_error("splitFirst: delimiter not found"); }
    return                         { source.substr(0, next), source.substr(next+1) };
}
void printVersion() {
    std::cout << "gringo version 4.0-rc2\n";
}
void printHelp() {
    printVersion();
    std::cout << 
        "\n"
        "Usage: gringo [options] [files]\n"
        "\n"
        "Gringo Options:\n"
        "\n"
        "  --text,-t        : Print plain text format\n"
        "  --lparse-rewrite : Use in conjunction with -t to inspect lparse rewriting.\n"
        "  -f <file>        : Explicitly pass a file name\n"
        "                     (e.g., file names starting with -)\n"
        "\n"
        "Gringo Warnings:\n"
        "\n"
        "  -Wno-atom-undefined        : a :- b.\n"
        "  -Wno-define-cyclic         : #const a=b. #const b=a.\n"
        "  -Wno-define-redfinition    : #const a=1. #const a=2.\n"
        "  -Wno-file-included         : #include \"a.lp\". #include \"a.lp\".\n"
        "  -Wno-nonmonotone-aggregate : a :- #sum { 1:a; -1:a } >= 0.\n"
        "  -Wno-term-undefined        : p(1/0).\n"
        "\n"
        "Basic Options:\n"
        "\n"
        "  --help,-h    : Print help information and exit\n"
        "  --version,-v : Print version information and exit\n"
        "  --verbose,-V : Print intermediate program representations\n"
        "\n"
        "Usage: gringo [options] [files]\n";
}
int main(int argc, char **argv) {
    using namespace Gringo;
    try {
        bool text          = false;
        bool lparseRewrite = false;
        Output::OutputPredicates outputPredicates;
        std::vector<char const *> files;
        char const *ACO = "--aspcomp13-output=";
        for (; argc > 1; argc--, argv++) {
            // TODO: remove
            if (!strncmp(argv[1], ACO, strlen(ACO))) {
                for (auto &x : split(argv[1]+strlen(ACO), ",")) {
                    auto y(splitFirst(x, "/"));
                    outputPredicates.emplace_back(Location("<cmd>",1,1,"<cmd>", 1,1), Signature(y.first, stoi(y.second)));
                }
            }
            else if (!strcmp(argv[1], "--lparse-rewrite"))                    { lparseRewrite = true; }
            else if (!strcmp(argv[1], "-t") || !strcmp(argv[1], "--text"))    { text          = true; }
            else if (!strcmp(argv[1], "-h") || !strcmp(argv[1], "--help"))    { printHelp(); std::exit(0); }
            else if (!strcmp(argv[1], "-v") || !strcmp(argv[1], "--version")) { printVersion(); std::exit(0); }
            else if (!strcmp(argv[1], "-V") || !strcmp(argv[1], "--verbose")) { g_verbose = true; }
            else if (!strcmp(argv[1], "-f"))                                  { files.emplace_back(argv[1]); }
            else if (!strcmp(argv[1], "-Wno-define-redfinition"))             { message_printer()->disable(W_DEFINE_REDEFINTION); }
            else if (!strcmp(argv[1], "-Wno-define-cyclic"))                  { message_printer()->disable(W_DEFINE_CYCLIC); }
            else if (!strcmp(argv[1], "-Wno-term-undefined"))                 { message_printer()->disable(W_TERM_UNDEFINED); }
            else if (!strcmp(argv[1], "-Wno-atom-undefined"))                 { message_printer()->disable(W_ATOM_UNDEFINED); }
            else if (!strcmp(argv[1], "-Wno-nonmonotone-aggregate"))          { message_printer()->disable(W_NONMONOTONE_AGGREGATE); }
            else if (!strcmp(argv[1], "-Wno-file-included"))                  { message_printer()->disable(W_FILE_INCLUDED); }
            else if (!strncmp(argv[1], "-", 1) && strnlen(argv[1], 2) > 1)    { printHelp(); throw std::runtime_error(std::string("unknow option: ") + argv[1]); }
            else                                                              { files.emplace_back(argv[1]); }
        }
        if (text) {
            Output::OutputBase out(std::cout, lparseRewrite);
            std::move(outputPredicates.begin(), outputPredicates.end(), std::back_inserter(out.outPreds));
            ground(out, files);
            // TODO: this should be moved into the output
            for (auto &x : out.outPreds) { 
                if (x.second != Signature("", 0)) { std::cout << "#show " << x.second.name() << "/" << x.second.length() << ".\n"; }
                else                              { std::cout << "#show.\n"; }
            }
        }
        else {
            Output::PlainLparseOutputter plo(std::cout);
            Output::OutputBase out(plo);
            std::move(outputPredicates.begin(), outputPredicates.end(), std::back_inserter(out.outPreds));
            ground(out, files);
            plo.finish(out.domains, out.outPreds);
        }
    }
    catch (std::exception &e) {
        std::cerr << "\n" << "Exception: " << e.what() << std::endl;
        std::exit(1);
    }
    return 0;
}

