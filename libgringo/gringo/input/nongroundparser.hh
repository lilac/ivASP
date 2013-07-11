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

#ifndef _GRINGO_INPUT_NONGROUNDPARSER_HH
#define _GRINGO_INPUT_NONGROUNDPARSER_HH

#include <gringo/input/programbuilder.hh>
#include <gringo/lexerstate.hh>
#include <memory>
#include <iosfwd>
#include <set>

namespace Gringo { namespace Input {

namespace NonGroundGrammar { class location; }

// {{{ declaration of NonGroundParser

class NonGroundParser : private LexerState {
public:
    NonGroundParser(INongroundProgramBuilder &pb);
    void parseError(Location const &loc, std::string const &token);
    void pushFile(std::string &&filename);
    void pushStream(std::string &&name, std::unique_ptr<std::istream>);
    int lex(void *pValue, NonGroundGrammar::location &loc);
    bool parse();
    void include(unsigned sUid, NonGroundGrammar::location const &loc);
    INongroundProgramBuilder &builder();
    // {{{ aggregate helper functions
    BoundVecUid boundvec(Relation ra, TermUid ta, Relation rb, TermUid tb);
    unsigned aggregate(AggregateFunction fun, unsigned choice, unsigned elems, BoundVecUid bounds);
    HdLitUid headaggregate(Location const &loc, unsigned hdaggr);
    BdLitVecUid bodyaggregate(BdLitVecUid body, Location const &loc, NAF naf, unsigned bdaggr);
    // }}}
    ~NonGroundParser();

private:
    int lex_impl(void *pValue, NonGroundGrammar::location &loc);
    void lexerError(std::string const &token);

private:
    std::set<std::string> filenames_;
    unsigned not_;
    INongroundProgramBuilder &pb_;
    struct Aggr
    {
        AggregateFunction fun;
        unsigned choice;
        unsigned elems;
        BoundVecUid bounds;
    };
    Indexed<Aggr> aggregates_;
};

// }}}

} } // namespace Input Gringo

#endif // _GRINGO_INPUT_NONGROUNDPARSER_HH
