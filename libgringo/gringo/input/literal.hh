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

#ifndef _GRINGO_INPUT_LITERAL_HH
#define _GRINGO_INPUT_LITERAL_HH

#include <gringo/term.hh>
#include <gringo/ground/literal.hh>
#include <vector>
#include <memory>

namespace Gringo { namespace Input {

// {{{ declaration of Literal

struct Literal;
struct LiteralVisitor;
struct LiteralConstVisitor;
typedef std::unique_ptr<Literal> ULit;
typedef std::vector<ULit> ULitVec;

struct Literal : Printable, Hashable, Locatable, Comparable<Literal>, Clonable<Literal> {
    typedef std::tuple<unsigned, std::vector<std::pair<UTerm, UTerm>>, std::unordered_map<Term*, FWString, value_hash<Term*>, value_equal_to<Term*>>> ProjectionMap;
    typedef std::vector<std::pair<UTerm, UTerm>> AssignVec;

    //! Removes all occurrences of PoolTerm instances. 
    //! Returns all unpooled incarnations of the literal.
    //! \note The literal becomes unusable after the method returns.
    //! \post The returned pool does not contain PoolTerm instances.
    virtual ULitVec unpool() const = 0;
    //! Simplifies the literal.
    virtual void simplify(ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum, bool positional = true) = 0;
    //! Collects variables.
    //! \pre Must be called after simplify to properly account for bound variables.
    virtual void collect(VarTermBoundVec &vars, bool bound) const = 0;
    //! Removes non-invertible arithmetics.
    //! \note This method will not be called for head literals.
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, AssignVec &assign, unsigned &auxNum) = 0;
    virtual void toTuple(UTermVec &tuple, int &id) = 0;
    virtual Value isEDB() const;
    virtual bool hasPool() const = 0;
    virtual void replace(Defines &dx) = 0;
    virtual Ground::ULit toGround(PredDomMap &x) const = 0;
    virtual ULit shift(bool negate) = 0;
    virtual UTerm headRepr() const = 0;
    virtual ~Literal() { }
};

// }}}
// {{{ declaration of PredicateLiteral

struct PredicateLiteral : Literal {
    PredicateLiteral(NAF naf, UTerm &&repr);
    virtual void collect(VarTermBoundVec &vars, bool bound) const;
    virtual void toTuple(UTermVec &tuple, int &id);
    virtual PredicateLiteral *clone() const;
    virtual void print(std::ostream &out) const;
    virtual bool operator==(Literal const &other) const;
    virtual size_t hash() const;
    virtual void simplify(ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum, bool positional = true);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, AssignVec &assign, unsigned &auxNum);
    virtual ULitVec unpool() const;
    virtual Value isEDB() const;
    virtual bool hasPool() const;
    virtual void replace(Defines &dx);
    virtual Ground::ULit toGround(PredDomMap &x) const;
    virtual ULit shift(bool negate);
    virtual UTerm headRepr() const;
    virtual ~PredicateLiteral();

    NAF naf;
    UTerm repr;
};

// }}}
// {{{ declaration of RelationLiteral

struct RelationLiteral : Literal {
    RelationLiteral(Relation rel, UTerm &&left, UTerm &&right);
    virtual void collect(VarTermBoundVec &vars, bool bound) const;
    virtual void toTuple(UTermVec &tuple, int &id);
    virtual RelationLiteral *clone() const;
    virtual void print(std::ostream &out) const;
    virtual bool operator==(Literal const &other) const;
    virtual size_t hash() const;
    virtual void simplify(ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum, bool positional = true);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, AssignVec &assign, unsigned &auxNum);
    virtual ULitVec unpool() const;
    virtual bool hasPool() const;
    virtual void replace(Defines &dx);
    virtual Ground::ULit toGround(PredDomMap &x) const;
    virtual UTerm headRepr() const;
    virtual ULit shift(bool negate);
    virtual ~RelationLiteral();
    static ULit make(Term::ArithmeticsMap::value_type::value_type &x);
    static ULit make(Literal::AssignVec::value_type &x);

    Relation rel;
    UTerm left;
    UTerm right;
};

// }}}
// {{{ declaration of RangeLiteral

struct RangeLiteral : Literal {
    RangeLiteral(UTerm &&assign, UTerm &&lower, UTerm &&upper);
    virtual void collect(VarTermBoundVec &vars, bool bound) const;
    virtual void toTuple(UTermVec &tuple, int &id);
    virtual RangeLiteral *clone() const;
    virtual void print(std::ostream &out) const;
    virtual bool operator==(Literal const &other) const;
    virtual size_t hash() const;
    virtual void simplify(ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum, bool positional = true);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, AssignVec &assign, unsigned &auxNum);
    virtual ULitVec unpool() const;
    virtual bool hasPool() const;
    virtual void replace(Defines &dx);
    virtual Ground::ULit toGround(PredDomMap &x) const;
    virtual ULit shift(bool negate);
    virtual UTerm headRepr() const;
    virtual ~RangeLiteral();
    static ULit make(Term::DotsMap::value_type &dot);

    UTerm assign;
    UTerm lower;
    UTerm upper;
};

// }}}
// {{{ declaration of FalseLiteral

struct FalseLiteral : Literal {
    FalseLiteral();
    virtual void collect(VarTermBoundVec &vars, bool bound) const;
    virtual void toTuple(UTermVec &tuple, int &id);
    virtual FalseLiteral *clone() const;
    virtual void print(std::ostream &out) const;
    virtual bool operator==(Literal const &other) const;
    virtual size_t hash() const;
    virtual void simplify(ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum, bool positional = true);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, AssignVec &assign, unsigned &auxNum);
    virtual ULitVec unpool() const;
    virtual bool hasPool() const;
    virtual void replace(Defines &dx);
    virtual Ground::ULit toGround(PredDomMap &x) const;
    virtual ULit shift(bool negate);
    virtual UTerm headRepr() const;
    virtual ~FalseLiteral();
};

// }}}

} } // namespace Input Gringo

GRINGO_HASH(Input::Literal)

#endif // _GRINGO_INPUT_LITERAL_HH
