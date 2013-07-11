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

#include "gringo/input/nongroundparser.hh"
#include "gringo/lexerstate.hh"
#include "gringo/value.hh"
#include "gringo/logger.hh"
#include "input/nongroundgrammar/grammar.hh"
#include <cstddef>
#include <climits>
#include <memory>
#include <fstream>
#include <vector>
#include <algorithm>

namespace Gringo { namespace Input {

// {{{ defintion of NonGroundParser

NonGroundParser::NonGroundParser(INongroundProgramBuilder &pb)
    : not_(FWString::uid("not"))
    , pb_(pb) { }

void NonGroundParser::parseError(Location const &loc, std::string const &msg) {
    // Note: bison generated C++ parser leaks memory...
    GRINGO_REPORT(ERROR) << loc << ": error: " << msg << "\n";
}

void NonGroundParser::lexerError(std::string const &token) {
    GRINGO_REPORT(ERROR) << filename() << ":" << line() << ":" << column() << ": error: lexer error, unexpected " << token << "\n";
}

void NonGroundParser::pushFile(std::string &&file) {
    auto res = filenames_.insert(std::forward<std::string>(file));
    if (!res.second) {
        GRINGO_REPORT(W_FILE_INCLUDED) << "<cmd>: warning: already included file ignored:\n"
            << "  " << *res.first << "\n";
    }
    else if (!push(*res.first)) {
        GRINGO_REPORT(ERROR) << "<cmd>: error: '" << *res.first << "' file could not be opened\n";
    }
}

void NonGroundParser::pushStream(std::string &&file, std::unique_ptr<std::istream> in) {
    auto res = filenames_.insert(std::move(file));
    if (!res.second) {
        GRINGO_REPORT(W_FILE_INCLUDED) << "<cmd>: warning: already included file ignored:\n"
            << "  " << *res.first << "\n";
    }
    else if (!push(*res.first, std::move(in))) {
        GRINGO_REPORT(ERROR) << "<cmd>: error: '" << *res.first << "' file could not be opened\n";
    }
}

int NonGroundParser::lex(void *pValue, NonGroundGrammar::location &loc) {
    while (!empty()) {
        int minor = lex_impl(pValue, loc);
        loc.end.filename = const_cast<std::string*>(&filename());
        loc.end.line = line();
        loc.end.column = column();
        if (minor) { return minor; }
        else { pop(); }
    }
    return 0;
}

void NonGroundParser::include(unsigned sUid, NonGroundGrammar::location const &loc) {
    auto res = filenames_.insert(*FWString(sUid));
    if (!res.second) {
        GRINGO_REPORT(W_FILE_INCLUDED) << loc << ": warning: already included file ignored:\n"
            << "  " << *res.first << "\n";
    }
    else if (!push(*res.first)) {
#if defined _WIN32 || defined __WIN32__ || defined __EMX__ || defined __DJGPP__
        const char *SLASH = "\\/";
#else
        const char *SLASH = "/";
#endif
        size_t slash = loc.begin.filename->find_last_of(SLASH);
        if (slash != std::string::npos) {
            std::string path = loc.begin.filename->substr(0, slash + 1);
            auto res2 = filenames_.insert(path + *res.first);
            if (!res2.second) {
                GRINGO_REPORT(W_FILE_INCLUDED) << loc << ": warning: already included file ignored:\n"
                    << "  " << *res2.first << "\n";
            }
            else if (!push(*res2.first)) {
                GRINGO_REPORT(ERROR) << loc << ": error: '" << *res2.first << "' file could not be opened\n";
            }
        }
    }
}

bool NonGroundParser::parse() {
    NonGroundGrammar::parser parser(this);
    return parser.parse() == 0;
}

INongroundProgramBuilder &NonGroundParser::builder() {
    return pb_;
}

unsigned NonGroundParser::aggregate(AggregateFunction fun, unsigned choice, unsigned elems, BoundVecUid bounds) {
    return aggregates_.insert({ fun, choice, elems, bounds});
}

HdLitUid NonGroundParser::headaggregate(Location const &loc, unsigned hdaggr) {
    auto aggr = aggregates_.erase(hdaggr);
    if (aggr.choice) { return builder().headaggr(loc, aggr.fun, aggr.bounds, CondLitVecUid(aggr.elems)); }
    else { return builder().headaggr(loc, aggr.fun, aggr.bounds, HdAggrElemVecUid(aggr.elems)); }
}

BdLitVecUid NonGroundParser::bodyaggregate(BdLitVecUid body, Location const &loc, NAF naf, unsigned bdaggr) {
    auto aggr = aggregates_.erase(bdaggr);
    if (aggr.choice) { return builder().bodyaggr(body, loc, naf, aggr.fun, aggr.bounds, CondLitVecUid(aggr.elems)); }
    else { return builder().bodyaggr(body, loc, naf, aggr.fun, aggr.bounds, BdAggrElemVecUid(aggr.elems)); }
}

BoundVecUid NonGroundParser::boundvec(Relation ra, TermUid ta, Relation rb, TermUid tb) {
    auto bound(builder().boundvec());
    auto undef = TermUid(-1);
    if (ta != undef) { builder().boundvec(bound, inv(ra), ta); }
    if (tb != undef) { builder().boundvec(bound, rb, tb); }
    return bound;
}

NonGroundParser::~NonGroundParser() { }

// }}}

} } // namespace Input Gringo

#include "input/nongroundlexer.hh"

