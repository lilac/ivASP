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

#ifndef _GRINGO_GROUND_LITERAL_HH
#define _GRINGO_GROUND_LITERAL_HH

#include <gringo/ground/dependency.hh>
#include <gringo/ground/instantiation.hh>
#include <gringo/output/literal.hh>

namespace Gringo { namespace Ground {

// {{{ declaration of HeadOccurrence

struct HeadOccurrence {
    virtual void defines(IndexUpdater &update, Instantiator *inst) = 0;
    virtual ~HeadOccurrence() { }
};

// }}}

// {{{ declaration of Literal

typedef BodyOccurrence<HeadOccurrence> BodyOcc;
struct Literal : Printable {
	enum Type {
		STATIC,
		CUMULATIVE,
		VOLATILE
	};
    using SValVec = Instantiator::SValVec;
    using Score   = double;
    virtual bool isRecursive() const = 0;
    virtual UIdx index(BinderType type, Term::VarSet &bound) = 0;
    virtual BodyOcc *occurrence() = 0;
    virtual void collect(VarTermBoundVec &vars) const = 0;
    virtual void collectImportant(Term::VarSet &vars);
    virtual Output::Literal *toOutput() = 0;
    virtual Score score(Term::VarSet const &bound) = 0;
    virtual bool isNew() { return false; }
    virtual Type incrType() const;
    virtual ~Literal() { }
};
typedef std::unique_ptr<Literal> ULit;
typedef std::vector<ULit> ULitVec;

// }}}

// {{{ declaration of RangeLiteral

using RangeLiteralShared = std::pair<UTerm, UTerm>;
struct RangeLiteral : Literal {
    RangeLiteral(Term const &assign, Term const &left, Term const &right);
    virtual void print(std::ostream &out) const;
    virtual bool isRecursive() const;
    virtual BodyOcc *occurrence();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual UIdx index(BinderType type, Term::VarSet &bound);
    virtual Output::Literal *toOutput();
    virtual Score score(Term::VarSet const &bound);
    virtual bool isNew();
    virtual Type incrType() const;
    virtual ~RangeLiteral();

    UTerm assign;
    RangeLiteralShared range;
};

// }}}
// {{{ declaration of RelationLiteral

using RelationShared = std::tuple<Relation, UTerm, UTerm>;
struct RelationLiteral : Literal {
    RelationLiteral(Relation rel, Term const &left, Term const &right);
    virtual void print(std::ostream &out) const;
    virtual bool isRecursive() const;
    virtual BodyOcc *occurrence();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual UIdx index(BinderType type, Term::VarSet &bound);
    virtual Output::Literal *toOutput();
    virtual Score score(Term::VarSet const &bound);
    virtual bool isNew();
    virtual Type incrType() const;
    virtual ~RelationLiteral();

    RelationShared shared;
};

// }}}
// {{{ declaration of PredicateLiteral

struct PredicateLiteral : Literal, BodyOcc {
    PredicateLiteral(PredicateDomain &dom, NAF naf, UTerm &&repr);
    virtual void print(std::ostream &out) const;
    virtual UGTerm getRepr() const;
    virtual bool isPositive() const;
    virtual bool isNegative() const;
    virtual void setType(OccurrenceType x);
    virtual OccurrenceType getType() const;
    virtual bool isRecursive() const;
    virtual BodyOcc *occurrence();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual DefinedBy &definedBy();
    virtual UIdx index(BinderType type, Term::VarSet &bound);
    virtual Output::Literal *toOutput();
    virtual Score score(Term::VarSet const &bound);
    virtual void checkDefined(LocSet &done, SigSet const &edb) const;
    virtual bool isNew();
    virtual Type incrType() const;
    virtual ~PredicateLiteral();

    OccurrenceType type = OccurrenceType::POSITIVELY_STRATIFIED;
    UTerm repr;
    DefinedBy defs;
    PredicateDomain &domain;
    Output::PredicateLiteral gLit;   
};

// }}}

} } // namespace Ground Gringo

#endif // _GRINGO_GROUND_LITERAL_HH

