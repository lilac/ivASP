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

#ifndef _GRINGO_GROUND_STATEMENT_HH
#define _GRINGO_GROUND_STATEMENT_HH

#include <gringo/ground/literal.hh>
#include <gringo/ground/instantiation.hh>
#include <gringo/ground/dependency.hh>
#include <gringo/output/statement.hh>
#include <gringo/intervals.hh>

namespace Gringo { namespace Ground {

// TODO: check if a has-a relation ship for bodyoccurrences would work too

// {{{ declaraion of HeadDefinition

struct HeadDefinition : HeadOccurrence {
    HeadDefinition(UTerm &&repr);
    HeadDefinition(HeadDefinition &&) = default;
    UGTerm getRepr() const;
    void collectImportant(Term::VarSet &vars);
    void enqueue(Queue &queue);
    virtual void defines(IndexUpdater &update, Instantiator *inst);
    virtual ~HeadDefinition();

    UTerm repr;
    typedef std::unordered_map<IndexUpdater*, unsigned> OffsetMap;
    typedef std::vector<std::reference_wrapper<Instantiator>> RInstVec;
    typedef std::vector<std::pair<IndexUpdater*, RInstVec>> EnqueueVec;
    OffsetMap offsets;
    EnqueueVec enqueueVec;
    bool active = false;
};
typedef std::vector<HeadDefinition> HeadDefVec;
using UHeadDef = std::unique_ptr<HeadDefinition>;

// }}}

// {{{ declaration of BodyAggregateDomain

using Output::BodyAggregateState;

struct BodyAggregateDomain : AbstractDomain<BodyAggregateState> {
    BodyAggregateDomain(UTerm &&repr, BoundVec &&bounds, AggregateFunction fun);
    void insert(Value const &repr, ValVec const &tuple, Output::LitVec const &cond, Location const &loc);
    virtual void mark();
    virtual void unmark();
    virtual ~BodyAggregateDomain();

    UTerm             repr;
    Location const   *loc;
    BoundVec          bounds;
    AggregateFunction fun;
    unsigned          numBlocked = 0;
    unsigned          blocked    = 0;
    unsigned          marks      = 0;
    bool              positive   = true;
};
typedef std::shared_ptr<BodyAggregateDomain> SBodyAggregateDomain;

// }}}
// {{{ declaration of AssignmentAggregateDomain

using Output::AssignmentAggregateState;

struct AssignmentAggregateDomain : AbstractDomain<AssignmentAggregateState> {
    using DataMap = std::unordered_map<Value, AssignmentAggregateState::Data>;
    using DataVec = std::vector<std::reference_wrapper<DataMap::value_type>>;

    AssignmentAggregateDomain(UTerm &&repr, UTerm &&specialRepr, AggregateFunction fun);
    void insert(Value const &repr, ValVec const &tuple, Output::LitVec const &cond, Location const &loc);
    template <class Vec>
    void insert(Vec const &values, DataMap::value_type &y);

    virtual void mark();
    virtual void unmark();

    virtual ~AssignmentAggregateDomain();

    DataMap           dataMap;
    DataVec           dataVec;
    UTerm             repr;
    UTerm             specialRepr;
    Location const   *loc;
    AggregateFunction fun;
    ValVec            valsCache;
    unsigned          numBlocked  = 0;
    unsigned          blocked     = 0;
    unsigned          marks       = 0;
    unsigned          offset      = 0;
};
typedef std::shared_ptr<AssignmentAggregateDomain> SAssignmentAggregateDomain;

// }}} 
// {{{ declaration of ConjunctionDomain

using Output::ConjunctionState;

struct ConjunctionHead : BodyOcc {
    ConjunctionHead(PredicateDomain &predDom, UTerm &&predRep, PredicateDomain &headDom, UTerm &&headRep)
        : predRep(std::move(predRep))
        , headRep(std::move(headRep))
        , predDef(get_clone(this->predRep))
        , predDom(predDom)
        , headDom(headDom)                              { }
        virtual UGTerm getRepr() const                  { return headRep->gterm(); }
        virtual bool isPositive() const                 { return true; };
        virtual bool isNegative() const                 { return false; };
        virtual void setType(OccurrenceType x)          { type = x; };
        virtual OccurrenceType getType() const          { return type; }
        virtual DefinedBy &definedBy()                  { return defines; }
        virtual void checkDefined(LocSet &, SigSet const &) const { }
        virtual ~ConjunctionHead()                      { }

    DefinedBy        defines;
    UTerm            predRep;
    UTerm            headRep;
    HeadDefinition   predDef;
    PredicateDomain &predDom;
    PredicateDomain &headDom;
    OccurrenceType   type    = OccurrenceType::POSITIVELY_STRATIFIED;
};

struct ConjunctionDomain : AbstractDomain<ConjunctionState> {
    using UConjHd = std::unique_ptr<ConjunctionHead>;

    ConjunctionDomain(UTerm &&domRepr, PredicateDomain *predDom, UTerm &&predRep, PredicateDomain *headDom, UTerm &&headRep, ULitVec &&lits);
    virtual void mark();
    virtual void unmark();
    void insert();
    void unblock(bool fact);
    void insertEmpty();
    virtual ~ConjunctionDomain();

    UTerm    rep;
    UConjHd  head;
    ULitVec  lits;
    unsigned numBlocked = 0;
    unsigned blocked    = 0;
    unsigned marks      = 0;
};
typedef std::shared_ptr<ConjunctionDomain> SConjunctionDomain;

// }}}
// {{{ declaration of HeadAggregateDomain

struct HeadDummyDep : HeadOccurrence, BodyOcc {
    HeadDummyDep(FWString name) : name(name)             { }
    virtual void defines(IndexUpdater &, Instantiator *) { }
    virtual UGTerm getRepr() const                       { return make_unique<GValTerm>(name); }
    virtual bool isPositive() const                      { return true; }
    virtual bool isNegative() const                      { return false; }
    virtual void setType(OccurrenceType)                 { }
    virtual OccurrenceType getType() const               { return OccurrenceType::POSITIVELY_STRATIFIED; }
    virtual DefinedBy &definedBy()                       { return def; }
    virtual void checkDefined(LocSet &, SigSet const &) const      { }
    virtual ~HeadDummyDep()                              { }

    FWString name;
    DefinedBy def;
};

using HeadAggregateState = Output::HeadAggregateState;
struct HeadAggregateDomain : AbstractDomain<HeadAggregateState> {
    using MarksQueue    = std::deque<unsigned>;
    using AccumulateVec = std::vector<std::pair<PredicateDomain&, HeadDefinition>>;
    using TodoVec       = std::vector<std::reference_wrapper<HeadAggregateState>>;

    HeadAggregateDomain(UTerm &&repr, AggregateFunction fun, BoundVec &&bounds, FWString dummy);
    HeadAggregateState &insert(Value rep);
    void accumulate(unsigned headNum, ValVec const &tuple, Output::LitVec const &lits, Location const &loc);
    AccumulateVec::value_type &head(unsigned x) { return heads[x-1]; }
    virtual void accumulateMark();
    virtual void accumulateUnmark(Queue &queue);
    virtual void mark();
    virtual void unmark();
    virtual ~HeadAggregateDomain();

    MarksQueue        marks;
    UTerm             repr;
    AggregateFunction fun;
    BoundVec          bounds;
    HeadDummyDep      dummy;
    AccumulateVec     heads;
    TodoVec           todo;
    unsigned          numBlocked = 0;
    unsigned          blocked    = 0;
};
typedef std::shared_ptr<HeadAggregateDomain> SHeadAggregateDomain;

// }}}
// {{{ declaration of DisjunctionDomain

using DisjunctionState = Output::DisjunctionState;
struct DisjunctionDomain : AbstractDomain<DisjunctionState> {
    using MarksQueue = std::deque<unsigned>;

    DisjunctionDomain(UTerm &&repr);
    DisjunctionState &insert(Value rep);
    void accumulate(PredicateDomain::element_type *head, Output::LitVec const &lits);
    virtual void mark();
    virtual void unmark();
    virtual ~DisjunctionDomain();

    MarksQueue        marks;
    UTerm             repr;
};
typedef std::shared_ptr<DisjunctionDomain> SDisjunctionDomain;

// }}}

// {{{ declaration of BodyAggregateLiteral

struct BodyAggregateLiteral : Literal, BodyOcc {
    BodyAggregateLiteral(SBodyAggregateDomain dom, NAF naf);
    virtual void print(std::ostream &out) const;
    virtual bool isRecursive() const;
    virtual BodyOcc *occurrence();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual UGTerm getRepr() const;
    virtual bool isPositive() const;
    virtual bool isNegative() const;
    virtual void setType(OccurrenceType x);
    virtual UIdx index(BinderType type, Term::VarSet &bound);
    virtual OccurrenceType getType() const;
    virtual DefinedBy &definedBy();
    virtual Score score(Term::VarSet const &bound);
    virtual Output::Literal *toOutput();
    virtual void checkDefined(LocSet &done, SigSet const &edb) const;
    virtual ~BodyAggregateLiteral();

    SBodyAggregateDomain dom;
    DefinedBy defs;
    Output::BodyAggregate gLit;
    OccurrenceType type = OccurrenceType::POSITIVELY_STRATIFIED;
};

// }}}
// {{{ declaration of AssignmentAggregateLiteral

struct AssignmentAggregateLiteral : Literal, BodyOcc {
    AssignmentAggregateLiteral(SAssignmentAggregateDomain dom);
    virtual void print(std::ostream &out) const;
    virtual bool isRecursive() const;
    virtual BodyOcc *occurrence();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual UGTerm getRepr() const;
    virtual bool isPositive() const;
    virtual bool isNegative() const;
    virtual void setType(OccurrenceType x);
    UIdx index(BinderType type, Term::VarSet &bound);
    virtual OccurrenceType getType() const;
    virtual DefinedBy &definedBy();
    virtual Score score(Term::VarSet const &bound);
    virtual Output::Literal *toOutput();
    virtual void checkDefined(LocSet &done, SigSet const &edb) const;
    virtual ~AssignmentAggregateLiteral();

    SAssignmentAggregateDomain dom;
    DefinedBy defs;
    Output::AssignmentAggregate gLit;
    OccurrenceType type = OccurrenceType::POSITIVELY_STRATIFIED;
};

// }}}
// {{{ declaration of ConjunctionLiteral

struct ConjunctionLiteral : Literal, BodyOcc {
    ConjunctionLiteral(SConjunctionDomain dom);
    virtual void print(std::ostream &out) const;
    virtual bool isRecursive() const;
    virtual BodyOcc *occurrence();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual UGTerm getRepr() const;
    virtual bool isPositive() const;
    virtual bool isNegative() const;
    virtual void setType(OccurrenceType x);
    virtual OccurrenceType getType() const;
    virtual DefinedBy &definedBy();
    virtual UIdx index(BinderType type, Term::VarSet &bound);
    virtual Score score(Term::VarSet const &bound);
    virtual Output::Literal *toOutput();
    virtual void checkDefined(LocSet &done, SigSet const &edb) const;
    virtual ~ConjunctionLiteral();

    SConjunctionDomain dom;
    DefinedBy defs;
    Output::Conjunction gLit;
    OccurrenceType type = OccurrenceType::POSITIVELY_STRATIFIED;
};

// }}}
// {{{ declaration of HeadAggregateLiteral

struct HeadAggregateLiteral : Literal, BodyOcc {
    HeadAggregateLiteral(SHeadAggregateDomain dom);
    virtual void print(std::ostream &out) const;
    virtual bool isRecursive() const;
    virtual BodyOcc *occurrence();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual UGTerm getRepr() const;
    virtual bool isPositive() const;
    virtual bool isNegative() const;
    virtual void setType(OccurrenceType x);
    virtual UIdx index(BinderType type, Term::VarSet &bound);
    virtual OccurrenceType getType() const;
    virtual DefinedBy &definedBy();
    virtual Score score(Term::VarSet const &bound);
    virtual Output::Literal *toOutput();
    virtual void checkDefined(LocSet &done, SigSet const &edb) const;
    virtual ~HeadAggregateLiteral();

    SHeadAggregateDomain dom;
    DefinedBy            defs;
    HeadAggregateDomain::element_type *gResult = nullptr;
    OccurrenceType       type = OccurrenceType::POSITIVELY_STRATIFIED;
};

// }}}
// {{{ declaration of DisjunctionLiteral

struct DisjunctionLiteral : Literal, BodyOcc {
    DisjunctionLiteral(SDisjunctionDomain dom);
    virtual void print(std::ostream &out) const;
    virtual bool isRecursive() const;
    virtual BodyOcc *occurrence();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual UGTerm getRepr() const;
    virtual bool isPositive() const;
    virtual bool isNegative() const;
    virtual void setType(OccurrenceType x);
    virtual UIdx index(BinderType type, Term::VarSet &bound);
    virtual OccurrenceType getType() const;
    virtual DefinedBy &definedBy();
    virtual Score score(Term::VarSet const &bound);
    virtual Output::Literal *toOutput();
    virtual void checkDefined(LocSet &done, SigSet const &edb) const;
    virtual ~DisjunctionLiteral();

    SDisjunctionDomain               dom;
    DefinedBy                        defs;
    DisjunctionDomain::element_type *gResult = nullptr;
    OccurrenceType                   type    = OccurrenceType::POSITIVELY_STRATIFIED;
};

// }}}

// {{{ declaration of Statement

struct Statement;
typedef std::unique_ptr<Statement> UStm;
typedef std::vector<UStm> UStmVec;
typedef std::vector<UStmVec> UStmVecVec;

struct Statement : Printable {
    typedef Dependency<UStm, HeadOccurrence> Dep;
    virtual bool isNormal() const = 0;
    virtual void analyze(Dep::Node &node, Dep &dep) = 0;
    virtual void startLinearize(bool active) = 0;
    virtual void linearize(bool positive) = 0;
    virtual void enqueue(Queue &q) = 0;
    virtual ~Statement() { }
};

// }}}

// {{{ declaration of BodyAggregateAccumulate

struct BodyAggregateAccumulate : Statement, SolutionCallback {
    // Note: relation literals are used to check bounds and if necessary accumulate neutral elements
    BodyAggregateAccumulate(SBodyAggregateDomain dom, UTermVec &&tuple, ULitVec &&lits, ULitVec &&auxLits);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void enqueue(Queue &q);
    virtual void print(std::ostream &out) const;
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;
    virtual ~BodyAggregateAccumulate();

    SBodyAggregateDomain dom; //!< domain of the aggregate
    HeadDefinition defines;
    UTermVec tuple;           //!< tuple to accumulate
    ULitVec  lits;            //!< condition literals
    ULitVec  auxLits;         //!< literals necessary for grounding
    InstVec  insts;
};

// }}}
// {{{ declaration of AssignmentAggregateAccumulate

struct AssignmentAggregateAccumulate : Statement, SolutionCallback {
    AssignmentAggregateAccumulate(SAssignmentAggregateDomain dom, UTermVec const &tuple, ULitVec &&lits, ULitVec &&auxLits);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void enqueue(Queue &q);
    virtual void print(std::ostream &out) const;
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;
    virtual ~AssignmentAggregateAccumulate();

    SAssignmentAggregateDomain dom;
    HeadDefinition             defines;
    UTermVec                   tuple;
    ULitVec                    lits;
    ULitVec                    auxLits;
    InstVec                    insts;
};

// }}}
// {{{ declaration of ConjunctionAccumulate

    /* representation
     *   input(V) :- input(U); edge(U,V); input(W) : edge(U,W), V != W; not observed(U;V).
     *     d1(U,V),           <~ input(U), edge(U,V), not observed(U;V).
     *     d1(U,V), d2(U,V,W) <~ input(U), edge(U,V), edge(U,W), V != W, not observed(U;V).
     *     d1(U,V)            <~ input(W), d2(U,V,W).
     *     input(V) :- edge(U,V); input(U); d1(U,V,W); not observed(U;V).
     * important variables
     *   d1(U,V) and input(W) as well as the usual candidates
     * the first rule handles the empty case
     * the second rules handles two cases
     *   if the body of the nested rule is not fact or the input(W) is fact
     *     a tuple d1(U,V) is accumulated 
     *     d1(U,V,W) an element is added to the aggregate (possibly applying optimizations)
     *   otherwise
     *     d1(U,V) is marked and must not be imported into the binders
     *     d2(U,V,W) is accumulated
     *     the mark corresponds is just an atom over input(W)
     * the third rule waits for head atoms of the nested rule to become true
     *   d1(U,V,W) is matches if it has no marked elements
     */

struct ConjunctionAccumulateEmpty : Statement, SolutionCallback {
    ConjunctionAccumulateEmpty(SConjunctionDomain conjDom, ULitVec &&auxLits);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void print(std::ostream &out) const;
    virtual void enqueue(Queue &q);
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;
    virtual ~ConjunctionAccumulateEmpty();

    SConjunctionDomain  conjDom;
    HeadDefinition      conjDef;
    ULitVec             auxLits;
    InstVec             insts;
};

struct ConjunctionAccumulate : Statement, SolutionCallback {
    ConjunctionAccumulate(SConjunctionDomain conjDom, ULitVec &&auxLits);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void print(std::ostream &out) const;
    virtual void enqueue(Queue &q);
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;
    virtual ~ConjunctionAccumulate();

    SConjunctionDomain conjDom;
    HeadDefinition     conjDef;
    ULitVec            auxLits;
    InstVec            insts;
};

struct ConjunctionAccumulateFact : Statement, SolutionCallback {
    ConjunctionAccumulateFact(SConjunctionDomain dom, ULitVec &&auxLits);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void print(std::ostream &out) const;
    virtual void enqueue(Queue &q);
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;
    virtual ~ConjunctionAccumulateFact();

    SConjunctionDomain  conjDom;
    HeadDefinition      conjDef; 
    ULitVec             auxLits; // contains just two literals
    InstVec             insts;
};

// }}}
// {{{ declaration of HeadAggregateAccumulate

struct HeadAggregateAccumulate : Statement, SolutionCallback {
    HeadAggregateAccumulate(SHeadAggregateDomain dom, UTermVec &&tuple, PredicateDomain *predDom, UTerm &&repr, ULitVec &&lits);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void print(std::ostream &out) const;
    virtual void enqueue(Queue &q);
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;
    virtual ~HeadAggregateAccumulate();

    SHeadAggregateDomain dom;
    UTermVec             tuple;
    unsigned             headNum;
    ULitVec              lits;
    InstVec              insts;
};

// }}} 
// {{{ declaration of DisjunctionAccumulate

struct DisjunctionAccumulate : Statement, SolutionCallback {
    DisjunctionAccumulate(SDisjunctionDomain dom, PredicateDomain *predDom, UTerm &&repr, ULitVec &&lits);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void print(std::ostream &out) const;
    virtual void enqueue(Queue &q);
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;
    virtual ~DisjunctionAccumulate();

    SDisjunctionDomain  dom;
    PredicateDomain    *predDom;
    UHeadDef            def;
    ULitVec             lits;
    InstVec             insts;
};

// }}} 

// {{{ declaration of HeadAggregateRule

struct HeadAggregateRule : Statement, SolutionCallback {
    HeadAggregateRule(SHeadAggregateDomain dom, ULitVec &&lits);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void print(std::ostream &out) const;
    virtual void enqueue(Queue &q);
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;
    virtual ~HeadAggregateRule();

    SHeadAggregateDomain dom;
    HeadDefinition       def;
    ULitVec              lits;
    InstVec              insts;
};

// }}}
// {{{ declaration of DisjunctionRule

struct DisjunctionRule : Statement, SolutionCallback {
    DisjunctionRule(SDisjunctionDomain dom, ULitVec &&lits);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void print(std::ostream &out) const;
    virtual void enqueue(Queue &q);
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;
    virtual ~DisjunctionRule();

    SDisjunctionDomain dom;
    HeadDefinition     def;
    ULitVec            lits;
    InstVec            insts;
};

// }}}
// {{{ declaration of Rule

struct Rule : Statement, SolutionCallback {
    Rule(PredicateDomain *domain, UTerm &&repr, ULitVec &&body);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void enqueue(Queue &q);
    virtual void print(std::ostream &out) const;
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;

    virtual ~Rule();

    PredicateDomain *domain; // TODO: should go into defines
    UHeadDef defines;
    ULitVec lits;
    InstVec insts;
};

// }}}
// {{{ declaration of WeakConstraint

struct WeakConstraint : Statement, SolutionCallback {
    WeakConstraint(UTermVec &&tuple, ULitVec &&body);
    virtual bool isNormal() const;
    virtual void analyze(Dep::Node &node, Dep &dep);
    virtual void startLinearize(bool active);
    virtual void linearize(bool positive);
    virtual void enqueue(Queue &q);
    virtual void print(std::ostream &out) const;
    virtual void mark();
    virtual void report(Output::OutputBase &out);
    virtual void unmark(Queue &queue);
    virtual void printHead(std::ostream &out) const;

    virtual ~WeakConstraint();

    UTermVec tuple;
    ULitVec  lits;
    InstVec  insts;
};

// }}}

} } // namespace Ground Gringo

#endif // _GRINGO_GROUND_STATEMENT_HH
