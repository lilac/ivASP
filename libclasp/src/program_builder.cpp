//
// Copyright (c) 2006, 2007, 2012 Benjamin Kaufmann
//
// This file is part of Clasp. See http://www.cs.uni-potsdam.de/clasp/
//
// Clasp is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// Clasp is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Clasp; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//

#if defined(_MSC_VER)
#pragma warning (disable : 4146) // unary minus operator applied to unsigned type, result still unsigned
#pragma warning (disable : 4996) // 'std::_Fill_n' was declared deprecated
#endif

#include <clasp/program_builder.h>
#include <clasp/solver.h>
#include <clasp/minimize_constraint.h>
#include <clasp/preprocessor.h>
#include <clasp/dependency_graph.h>
#include <clasp/clause.h>
#include <sstream>
#include <climits>
#if defined(_MSC_VER)
#	if !defined(__FUNCTION__)
#	define MY_FUNCNAME __FILE__
#	else
# define MY_FUNCNAME __FUNCTION__
# endif
#elif defined(__GNUC__)
#define MY_FUNCNAME __PRETTY_FUNCTION__
#else
#define MY_FUNCNAME __FILE__
#endif

std::string precondition_error(const char* ex, const char* func, unsigned line) {
	std::stringstream err;
	err << func << "@" << line << ": precondition violated: " << ex;
	return err.str();
}

#define check_precondition(x, EX) \
	(void)( (!!(x)) || (throw EX(precondition_error((#x), MY_FUNCNAME, __LINE__)), 0))

namespace Clasp {
/////////////////////////////////////////////////////////////////////////////////////////
// class PreproStats
/////////////////////////////////////////////////////////////////////////////////////////
void PreproStats::reset() {
	std::memset(this, 0, sizeof(PreproStats));
}
uint32 PreproStats::sumRules() const {
	uint32 sum = 0;
	for (uint32 i = 0; i != NUM_RULE_TYPES; ++i) { sum += rules_[i].second; }
	return sum;
}
void PreproStats::accu(const PreproStats& o) {
	bodies   += o.bodies;
	atoms    += o.atoms;
	auxAtoms += o.auxAtoms;
	ufsNodes += o.ufsNodes;
	if (sccs == PrgNode::noScc || o.sccs == PrgNode::noScc) {
		sccs    = o.sccs;
		nonHcfs = o.nonHcfs;
	}
	else {
		sccs   += o.sccs;
		nonHcfs+= o.nonHcfs;
	}
	for (int i = 0; i != sizeof(eqs_)/sizeof(eqs_[0]); ++i) {
		eqs_[i] += o.eqs_[i];
	}
	for (int i = 0; i != sizeof(rules_)/sizeof(rules_[0]); ++i) {
		rules_[i].first  += o.rules_[i].first;
		rules_[i].second += o.rules_[i].second;
	}
}
/////////////////////////////////////////////////////////////////////////////////////////
// class ProgramBuilder
/////////////////////////////////////////////////////////////////////////////////////////
namespace {
struct LessBodySize {
	LessBodySize(const BodyList& bl) : bodies_(&bl) {}
	bool operator()(Var b1, Var b2 ) const {
		return (*bodies_)[b1]->size() < (*bodies_)[b2]->size()
			|| ((*bodies_)[b1]->size() == (*bodies_)[b2]->size() && (*bodies_)[b1]->type() < (*bodies_)[b2]->type());
	}
private:
	const BodyList* bodies_;
};

// Adds nogoods representing this node to the solver.
template <class NT>
bool toConstraint(NT* node, const ProgramBuilder& prg, ClauseCreator& c) {
	if (node->value() != value_free && !prg.context()->addUnary(node->trueLit())) {
    	return false;
	}
	return !node->relevant() || node->addConstraints(prg, c);
}
}
ProgramBuilder::ProgramBuilder() : minimize_(0), incData_(0), frozen_(true), ok_(true) { }
ProgramBuilder::~ProgramBuilder() { disposeProgram(true); }
ProgramBuilder::Incremental::Incremental() : startAtom_(1), startAux_(1), startScc_(0) {}

void ProgramBuilder::disposeProgram(bool force) {
	// remove rules
	std::for_each( bodies_.begin(), bodies_.end(), DestroyObject() );
	std::for_each( disjunctions_.begin(), disjunctions_.end(), DestroyObject() );
	AtomList().swap(sccAtoms_);
	BodyList().swap(bodies_);
	DisjList().swap(disjunctions_);
	bodyIndex_.clear();
	disjIndex_.clear();
	MinimizeRule* r = minimize_;
	while (r) {
		MinimizeRule* t = r;
		r = r->next_;
		delete t;
	}
	minimize_ = 0;
	for (RuleList::size_type i = 0; i != extended_.size(); ++i) {
		delete extended_[i];
	}
	extended_.clear();
	VarVec().swap(initialSupp_);
	rule_.clear();
	if (force) {
		std::for_each( atoms_.begin(), atoms_.end(), DeleteObject() );
		AtomList().swap(atoms_);
		delete incData_;
		VarVec().swap(propQ_);
		ruleState_.clearAll();
	}
	else {
		// clean up atoms
		// reset prop queue
		propQ_.assign(1, 0);
		uint32 startAux = incData_ ? incData_->startAux_ : (uint32)atoms_.size();
		assert(startAux <= atoms_.size());
		// remove rule associations
		for (VarVec::size_type i = 1; i != startAux; ++i) {
			PrgAtom* a = atoms_[i];
			// remove any dangling references
			a->clearSupports();
			a->clearDeps(PrgAtom::dep_all);
			a->setIgnoreScc(false);
			if (a->eq() && getEqAtom(i) >= startAux) {
				// atom i is equivalent to some aux atom 
				// make i the new root
				PrgAtom* eq = atoms_[getEqAtom(i)];
				assert(!eq->eq());
				eq->setEq(i);
				a->resetId(i, false);
				a->setLiteral(eq->literal());
			}
			if (!a->relevant()) { continue; }
			ValueRep v = a->value();
			a->setValue(value_free);
			if (!a->frozen()) {
				a->resetId(i, false);
				if (context()->master()->value(a->var()) != value_free) {
					v = context()->master()->isTrue(a->literal()) ? value_true : value_false;
				} 
				assert(!a->eq() || a->id() < startAux);
			}
			if (v != value_free) { assignValue(a, v); }
		}
		// delete any introduced aux atoms
		// this is safe because aux atoms are never part of the input program
		// it is necessary in order to free their ids, i.e. the id of an aux atom
		// from step I might be needed for a program atom in step I+1
		for (VarVec::size_type i = startAux; i != atoms_.size(); ++i) {
			delete atoms_[i];
		}
		atoms_.erase(atoms_.begin()+startAux, atoms_.end());
	}
	activeHead_.clear();
	activeBody_.reset();
	stats.reset();
}

ProgramBuilder& ProgramBuilder::startProgram(SharedContext& ctx, const EqOptions& eqOpts) {
	disposeProgram(true);
	// atom 0 is always false
	atoms_.push_back( new PrgAtom(0, false) );
	assignValue(getAtom(0), value_false);
	incData_  = 0;
	eqOpts_   = eqOpts;
	ctx_      = &ctx;
	ctx_->symTab().clear();
	ctx_->symTab().startInit();
	nonHcfCfg_= 0;
	frozen_   = false;
	ok_       = true;
	if (eqOpts_.noSCC) {
		stats.sccs    = PrgNode::noScc;
	}
	return *this;
}

ProgramBuilder& ProgramBuilder::updateProgram() {
	check_precondition(frozen_ || !incData_, std::logic_error);
	check_precondition(!atoms_.empty() && "startProgram() not called!", std::logic_error);
	if (!incData_)  { incData_ = new Incremental(); }
	// delete bodies/disjunctions...
	disposeProgram(false);
	incData_->startAtom_= (uint32)atoms_.size();
	incData_->startAux_ = (uint32)atoms_.size();
	incData_->unfreeze_.clear();
	frozen_   = false;
	ctx_->symTab().startInit();
	// add supported atoms from previous steps 
	// {ai | ai in P}.
	PrgBody* support = incData_->startAux_ > 1 ? getBodyFor(activeBody_) : 0;
	for (VarVec::size_type i = 1; i != incData_->startAux_; ++i) {
		PrgAtom* a = atoms_[i];
		if (a->relevant() && !a->frozen() && a->value() != value_false) {
			a->setIgnoreScc(true);
			support->addHead(a, PrgEdge::CHOICE_EDGE);
		}
	}
	return *this;
}

bool ProgramBuilder::endProgram() {
	return frozen_ || (prepareProgram(!eqOpts_.noSCC) && addConstraints());
}

bool ProgramBuilder::cloneProgram(SharedContext& ctx) {
	assert(frozen_);
	if (&ctx == ctx_) {
		return true;
	}
	ctx.copyVars(*ctx_);
	SharedContext* t = ctx_;
	ctx_   = &ctx;
	bool r = addConstraints();
	ctx_   = t;
	return r;
}

void ProgramBuilder::addMinimize(MinimizeBuilder& m) {
	check_precondition(frozen_, std::length_error);
	if (hasMinimize()) {
		WeightLitVec lits;
		for (MinimizeRule* r = minimize_; r; r = r->next_) {
			for (WeightLitVec::iterator it = r->lits_.begin(); it != r->lits_.end(); ++it) {
				PrgAtom* h = resize(it->first.var()); // checks for eq
				lits.push_back(WeightLiteral(it->first.sign() ? ~h->literal() : h->literal(), it->second));
			}
			m.addRule(lits);
			lits.clear();
		}
	}
}

void ProgramBuilder::writeProgram(std::ostream& os) {
	const char* const delimiter = "0";
	// first write all minimize rules - revert order!
	PodVector<MinimizeRule*>::type mr;
	for (MinimizeRule* r = minimize_; r; r = r->next_) {
		mr.push_back(r);
	}
	std::stringstream body;
	for (PodVector<MinimizeRule*>::type::reverse_iterator rit = mr.rbegin(); rit != mr.rend(); ++rit) {
		MinimizeRule* r = *rit;
		transform(*r, activeBody_);
		writeBody(activeBody_, body);
		os << OPTIMIZERULE << " " << 0 << " " << body.str() << "\n";
		body.clear();
		body.str("");
	}
	std::stringstream choice;
	uint32 numChoice = 0;
	uint32 falseAtom = 0;
	bool  oldFreeze  = frozen_;
	frozen_          = false;
	// write all bodies together with their heads
	for (BodyList::iterator it = bodies_.begin(); it != bodies_.end(); ++it) {
		PrgBody* b = *it;
		if (b->relevant() && (b->hasVar() || b->value() == value_false)) {
			transform(*b, activeBody_);
			writeBody(activeBody_, body);
			if (b->hasHeads() && b->value() != value_false) {
				for (PrgBody::head_iterator it = b->heads_begin(); it != b->heads_end(); ++it) {
					PrgHead* head = getHead(*it);
					if (head->hasVar()) {
						if (it->isAtom()) {
							if (it->isNormal()) {
								os << activeBody_.ruleType() << " " << it->node() << " " << body.str() << "\n";
							}
							else if (it->isChoice()) {
								choice << it->node() << " ";
								++numChoice;
							}
						}
						else if (it->isDisj()) {
							PrgDisj* d = static_cast<PrgDisj*>(head);
							os << DISJUNCTIVERULE << " " << d->size() << " ";
							for (PrgDisj::atom_iterator a = d->begin(), aEnd = d->end(); a != aEnd; ++a) {
								os << a->node() << " ";
							}
							os << body.str() << "\n";
						}
					}
				}
				if (numChoice) {
					os << CHOICERULE << " " << numChoice << " " << choice.str() << body.str() << "\n";
				}
			}
			else if (b->value() == value_false) {
				// write integrity constraint
				if (falseAtom == 0) {
					falseAtom = newAtom();
					setCompute(falseAtom, false);
				}
				os << activeBody_.ruleType() << " " << falseAtom << " " << body.str() << "\n";
			}
			body.clear();
			body.str("");
			choice.clear(); choice.str(""); numChoice = 0;
		}
	}
	// write eq-atoms, symbol-table and compute statement
	std::stringstream bp, bm, symTab;
	Literal comp;
	SymbolTable::const_iterator sym = ctx_->symTab().begin();
	for (AtomList::size_type i = 1; i < atoms_.size(); ++i) {
		// write the equivalent atoms
		if (atoms_[i]->eq()) {
			os << "1 " << i << " 1 0 " << getEqAtom(Var(i)) << " \n";
		}
		if ( (i == falseAtom || atoms_[i]->inUpper()) && atoms_[i]->value() != value_free ) {
			std::stringstream& str = atoms_[i]->value() == value_false ? bm : bp;
			str << i << "\n";
		}
		if (sym != ctx_->symTab().end() && Var(i) == sym->first) {
			if (sym->second.lit != negLit(sentVar) && !sym->second.name.empty()) {
				symTab << i << " " << sym->second.name.c_str() << "\n";
			}
			++sym;
		}
	}
	os << delimiter << "\n";
	os << symTab.str();
	os << delimiter << "\n";
	os << "B+\n" << bp.str() << "0\n"
		 << "B-\n" << bm.str() << "0\n1\n";
	frozen_ = oldFreeze;
}

/////////////////////////////////////////////////////////////////////////////////////////
// Program mutating functions
/////////////////////////////////////////////////////////////////////////////////////////
#define check_not_frozen() check_precondition(!frozen_ && "Can't update frozen program!", std::logic_error)
Var ProgramBuilder::newAtom() {
	check_not_frozen();
	Var id = static_cast<Var>(atoms_.size());
	atoms_.push_back( new PrgAtom(id) );
	return id;
}

ProgramBuilder& ProgramBuilder::setAtomName(Var atomId, const char* name) {
	check_not_frozen();
	check_precondition(atomId >= startAtom(), RedefinitionError);
	resize(atomId);
	ctx_->symTab().addUnique(atomId, name);
	return *this;
}

ProgramBuilder& ProgramBuilder::setCompute(Var atomId, bool pos) {
	resize(atomId);
	ValueRep v = pos ? value_weak_true : value_false;
	PrgAtom* a = atoms_[atomId];
	assert(!a->hasVar() || a->frozen());
	assignValue(a, v);
	return *this;
}

ProgramBuilder& ProgramBuilder::freeze(Var atomId) {
	check_not_frozen();
	check_precondition(incData_ && "ProgramBuilder::updateProgram() not called!", std::logic_error);
	PrgAtom* a = resize(atomId);
	if (atomId >= startAtom() && !a->frozen() && !a->supports()) {
		incData_->freeze_.push_back(atomId);
		a->setFrozen(true);
	}
	// else: atom is defined or from a previous step - ignore!
	return *this;
}

ProgramBuilder& ProgramBuilder::unfreeze(Var atomId) {
	check_not_frozen();
	check_precondition(incData_ && "ProgramBuilder::updateProgram() not called!", std::logic_error);
	PrgAtom* atom = resize(atomId);
	if (atomId >= startAtom() || atom->frozen()) {
		incData_->unfreeze_.push_back(atomId);
	}
	// else: atom is from a previous step - ignore!
	return *this;
}

ProgramBuilder& ProgramBuilder::addRule(const PrgRule& r) {
	check_not_frozen();
	// simplify rule
	RuleType t = simplifyRule(r, activeHead_, activeBody_);
	if (t != ENDRULE) { // rule is relevant
		upRules(t, 1);
		if (handleNatively(t, activeBody_)) { // and can be handled natively
			addRuleImpl(t, activeHead_, activeBody_);
		}
		else {
			bool     aux  = transformNoAux(t, activeBody_) == false;
			PrgRule* temp = new PrgRule();
			temp->setType(t);
			temp->setBound(activeBody_.bound());
			temp->heads.swap(activeHead_);
			temp->body.swap(activeBody_.lits);
			if (aux) {
				// Since rule transformation needs aux atoms, we must
				// defer the transformation until all rules were added
				// because only then we can safely assign new unique consecutive atom ids.
				extended_.push_back(temp);
			}
			else {
				PrgRuleTransform rt;
				incTr(t, rt.transformNoAux(*this, *temp));
				delete temp;
			}
		}
	}
	activeBody_.reset();
	return *this;
}
#undef check_not_frozen
/////////////////////////////////////////////////////////////////////////////////////////
// Query functions
/////////////////////////////////////////////////////////////////////////////////////////
Literal ProgramBuilder::getLiteral(Var atomId) const {
	check_precondition(atomId < atoms_.size(), std::logic_error);
	return getAtom(getEqAtom(atomId))->literal();
}
void ProgramBuilder::getAssumptions(LitVec& out) const {
	check_precondition(frozen_, std::logic_error);
	if (incData_) {
		for (VarVec::const_iterator it = incData_->freeze_.begin(), end = incData_->freeze_.end(); it != end; ++it) {
			out.push_back( ~getLiteral(*it) );
		}
	}
}
/////////////////////////////////////////////////////////////////////////////////////////
// Program definition - private
/////////////////////////////////////////////////////////////////////////////////////////
void ProgramBuilder::addRuleImpl(RuleType r, const VarVec& heads, BodyInfo& body) {
	if (r != OPTIMIZERULE) {
		assert(!heads.empty() && (r != DISJUNCTIVERULE || heads.size() > 1));
		PrgBody* b = getBodyFor(body);
		// only a non-false body can define atoms
		if (b->value() != value_false) {
			EdgeType t      = r != CHOICERULE ? PrgEdge::NORMAL_EDGE : PrgEdge::CHOICE_EDGE;
			uint32 headHash = 0;
			bool ignoreScc  = eqOpts_.noSCC || b->size() == 0;
			for (VarVec::const_iterator it = heads.begin(), end = heads.end(); it != end; ++it) {
				PrgAtom* a = resize(*it);
				check_precondition(*it >= startAtom() || a->frozen() || a->value() == value_false, RedefinitionError);
				if (a->frozen() && a->supports() == 0) { unfreeze(*it); }
				if (r != DISJUNCTIVERULE) {
					// Note: b->heads may now contain duplicates. They are removed in PrgBody::simplifyHeads.
					b->addHead(a, t);
					if (ignoreScc) { a->setIgnoreScc(ignoreScc); }
				}
				else {
					headHash += hashId(*it);
					ruleState_.addToHead(*it);
				}
			}
			if (r == DISJUNCTIVERULE) {
				assert(headHash != 0);
				PrgDisj* d = getDisjFor(heads, headHash);
				b->addHead(d, t);
			}
		}	
	}
	else {
		check_precondition(heads.empty(), std::logic_error);
		ProgramBuilder::MinimizeRule* mr = new ProgramBuilder::MinimizeRule;
		mr->lits_ = body.lits;
		mr->next_ = minimize_;
		minimize_ = mr;
	}
}

bool ProgramBuilder::assignValue(PrgAtom* a, ValueRep v) {
	if (a->eq())            { a = getAtom(getEqAtom(a->id())); }
	ValueRep old = a->value();
	if (old == value_weak_true && v != value_weak_true) old = value_free;
	if (!a->assignValue(v)) { setConflict(); return false; }
	if (old == value_free)  { propQ_.push_back(a->id()); }
	return true;
}
bool ProgramBuilder::assignValue(PrgHead* h, ValueRep v) {
	return !h->isAtom() || assignValue(static_cast<PrgAtom*>(h), v);
}

bool ProgramBuilder::handleNatively(RuleType r, const BodyInfo& body) const {
	ExtendedRuleMode m = eqOpts_.erMode;
	if (m == mode_native || m == mode_transform_integ || r == BASICRULE || r == OPTIMIZERULE) {
		return true;
	}
	else if (m == mode_transform) {
		return r == DISJUNCTIVERULE;
	}
	else if (m == mode_transform_dynamic) {
		return (r != CONSTRAINTRULE && r != WEIGHTRULE)
			|| transformNoAux(r, body) == false;
	}
	else if (m == mode_transform_choice) {
		return r != CHOICERULE;
	}
	else if (m == mode_transform_card)   {
		return r != CONSTRAINTRULE;
	}
	else if (m == mode_transform_weight) {
		return r != CONSTRAINTRULE && r != WEIGHTRULE;
	}
	assert(false && "unhandled extended rule mode");
	return true;
}

bool ProgramBuilder::transformNoAux(RuleType r, const BodyInfo& body) const {
	return r != CHOICERULE && (body.bound() == 1 || (body.size() <= 6 && choose(body.size(), body.bound()) <= 15));
}

void ProgramBuilder::transformExtended() {
	uint32 a   = numAtoms();
	if (incData_) {
		// remember starting position of aux atoms so
		// that we can remove them on next incremental step
		incData_->startAux_ = (uint32)atoms_.size();
	}
	PrgRuleTransform tm;
	for (RuleList::size_type i = 0; i != extended_.size(); ++i) {
		incTr(extended_[i]->type(), tm.transform(*this, *extended_[i]));
		delete extended_[i];
	}
	extended_.clear();
	incTrAux(numAtoms() - a);
}

void ProgramBuilder::transformIntegrity(uint32 maxAux) {
	if (stats.rules(CONSTRAINTRULE).second == 0) { return; }
	// find all constraint rules that are integrity constraints
	BodyList integrity;
	for (uint32 i = 0, end = static_cast<uint32>(bodies_.size()); i != end; ++i) {
		PrgBody* b = bodies_[i];
		if (b->relevant() && b->type() == BodyInfo::COUNT_BODY && b->value() == value_false) {
			integrity.push_back(b);
		}
	}
	if (!integrity.empty() && (integrity.size() == 1 || (atoms_.size()/double(bodies_.size()) > 0.5 && integrity.size() / double(bodies_.size()) < 0.01))) {
		uint32 A      = static_cast<uint32>(atoms_.size());
		Var falseAtom = 0;
		// transform integrity constraints
		for (BodyList::size_type i = 0; i != integrity.size(); ++i) {
			PrgBody* b = integrity[i];
			uint32 est = b->bound()*( b->sumW()-b->bound() );
			if (est > maxAux) { 
				// reached limit on aux atoms - stop transformation
				break; 
			}
			maxAux -= est;
			// transform rule
			PrgRule* r;
			extended_.push_back(r = new PrgRule());
			r->setType(CONSTRAINTRULE);
			r->setBound(b->bound());
			r->addHead(falseAtom);
			for (uint32 g = 0; g != b->size(); ++g) {
				r->addToBody(b->goal(g).var(), !b->goal(g).sign());
			}
			frozen_       = false;
			transformExtended();
			frozen_       = true;
			// propagate integrity condition to new rules
			propQ_.push_back(falseAtom);
			propagate(true);
			b->markRemoved();
		}
		// create vars for new atoms/bodies
		for (uint32 i = A; i != atoms_.size(); ++i) {
			PrgAtom* a = atoms_[i];
			for (PrgAtom::sup_iterator it = a->supps_begin(); it != a->supps_end(); ++it) {
				PrgBody* nb = bodies_[it->node()];
				assert(nb->value() != value_false);
				nb->assignVar(*this);
			}
			a->assignVar(*this, a->supports() ? *a->supps_begin() : PrgEdge::noEdge());	
		}
	}
}

// replace equivalent atoms in minimize rules
void ProgramBuilder::simplifyMinimize() {
	assert(hasMinimize());
	for (ProgramBuilder::MinimizeRule* r = minimize_; r; r = r->next_) {
		for (WeightLitVec::iterator it = r->lits_.begin(); it != r->lits_.end(); ++it) {
			it->first = Literal(getEqAtom(it->first.var()), it->first.sign());
		}
	}
}

void ProgramBuilder::updateFrozenAtoms() {
	if (incData_ != 0) {
		// remove protection of frozen atoms
		VarVec::iterator j = incData_->unfreeze_.begin();
		for (VarVec::iterator it = incData_->unfreeze_.begin(), end = incData_->unfreeze_.end(); it != end; ++it) {
			Var id     = getEqAtom(*it);
			PrgAtom* a = getAtom(id);
			if (a->frozen() && id < startAtom()) {
				assert(a->id() == id && a->scc() == PrgNode::noScc && !a->inUpper());
				a->markSeen(false);
				a->markDirty();
				*j++ = *it;
			}
			a->setFrozen(false);
		}
		incData_->unfreeze_.erase(j, incData_->unfreeze_.end());
		// atoms still frozen in this step
		j = incData_->freeze_.begin();
		activeHead_.clear();
		activeBody_.reset();
		PrgBody* support = 0;
		for (VarVec::iterator it = j, end = incData_->freeze_.end(); it != end; ++it) {
			PrgAtom* a = getAtom(*it);
			if (a->eq()) {
				*it = getEqAtom(*it);
				a   = getAtom(*it);
			}
			if (a->frozen()) {
				assert(a->relevant() && a->supports() == 0);
				a->resetId(*it, false);
				*j++ = *it;
				if (!support) { support = getBodyFor(activeBody_); }
				a->setIgnoreScc(true);
				support->addHead(a, PrgEdge::CHOICE_EDGE);
			}
		}
		incData_->freeze_.erase(j, incData_->freeze_.end());
	}
}

bool ProgramBuilder::prepareProgram(bool checkSccs) {
	assert(!frozen_);
	transformExtended();
	if (eqOpts_.normalize) { /* normalize(); */ assert(false);  }
	stats.atoms = numAtoms() - (startAtom()-1);
	stats.bodies= numBodies();
	updateFrozenAtoms();
	frozen_ = true;
	if (hasConflict() || !propagate(true)) {
		setConflict();
		return false;
	}
	Preprocessor p;
	if (!p.preprocess(*this, eqOpts_.iters != 0 ? Preprocessor::full_eq : Preprocessor::no_eq, eqOpts_.iters, eqOpts_.dfOrder)) {
		setConflict();
		return false;
	}
	if (eqOpts_.erMode == mode_transform_integ || eqOpts_.erMode == mode_transform_dynamic) {
		transformIntegrity(std::min(uint32(15000), uint32(numAtoms())<<1));
	}
	if (hasMinimize() && eqOpts_.iters != 0) {
		simplifyMinimize();
	}
	uint32 sccs = 0;
	if (checkSccs) {
		uint32 startScc = incData_ ? incData_->startScc_ : 0;
		PrgSccChecker c(*this, sccAtoms_, startScc);
		sccs       = c.sccs();
		stats.sccs = (sccs-startScc);
		if (incData_) { incData_->startScc_ = c.sccs(); }
	}
	finalizeDisjunctions(p, sccs);
	prepareNonHcfComponents();
	stats.atoms = numAtoms() - (startAtom()-1);
	bodyIndex_.clear();
	disjIndex_.clear();
	return true;
}

// replace disjunctions with gamma (shifted) and delta (component-shifted) rules
void ProgramBuilder::finalizeDisjunctions(Preprocessor& p, uint32 numSccs) {
	if (disjunctions_.empty()) { return; }
	// reset node ids changed by scc checking
	for (uint32 i = 0; i != bodies_.size(); ++i) {
		if (getBody(i)->relevant()) { getBody(i)->resetId(i, true); }
	}
	for (uint32 i = startAtom(); i != atoms_.size(); ++i) {
		if (getAtom(i)->relevant()) { getAtom(i)->resetId(i, true); }
	}
	VarVec head; BodyList supports;
	disjIndex_.clear();
	SccMap sccMap;
	sccMap.resize(numSccs, 0);
	enum SccFlag { seen_scc = 1u, is_scc_non_hcf = 128u };
	// replace disjunctions with shifted rules and non-hcf-disjunctions
	DisjList temp; temp.swap(disjunctions_);
	frozen_ = false;
	uint32 shifted = 0, added = 0;
	for (uint32 i = 0, end = temp.size(); i != end; ++i) {
		PrgDisj* d = temp[i];
		Literal dx = d->inUpper() ? d->literal() : negLit(0);
		PrgEdge e  = PrgEdge::newEdge(i, PrgEdge::CHOICE_EDGE, PrgEdge::DISJ_NODE);
		d->resetId(i, true); // id changed during scc checking
		// remove from program and 
		// replace with shifted rules or component-shifted disjunction
		head.clear(); supports.clear();
		for (PrgDisj::atom_iterator it = d->begin(), end = d->end(); it != end; ++it) {
			PrgAtom* at = getAtom(it->node());
			at->removeSupport(e);
			if (at->inUpper()) { 
				head.push_back(it->node()); 
				if (at->scc() != PrgNode::noScc) { sccMap[at->scc()] = seen_scc; }
			}
		}
		EdgeVec temp;
		d->clearSupports(temp);
		for (EdgeVec::iterator it = temp.begin(), end = temp.end(); it != end; ++it) {
			PrgBody* b = getBody(it->node());
			if (b->relevant() && b->value() != value_false) { supports.push_back(b); }
			b->removeHead(d, PrgEdge::NORMAL_EDGE);
		}
		d->destroy();
		// create shortcut for supports to avoid duplications during shifting
		Literal supportLit = dx != negLit(0) ? getEqAtomLit(dx, supports, p, sccMap) : dx;
		// create shifted rules and split disjunctions into non-hcf components
		for (VarVec::iterator hIt = head.begin(), hEnd = head.end(); hIt != hEnd; ++hIt) {
			uint32 scc = getAtom(*hIt)->scc();
			if (scc == PrgNode::noScc || (sccMap[scc] & seen_scc) != 0) {
				if (scc != PrgNode::noScc) { sccMap[scc] &= ~seen_scc; }
				else                       { scc = UINT32_MAX; }
				rule_.clear(); rule_.setType(DISJUNCTIVERULE);
				rule_.addHead(*hIt);
				if (supportLit.var() != 0) { rule_.addToBody(supportLit.var(), !supportLit.sign()); }
				else if (supportLit.sign()){ continue; }
				for (VarVec::iterator oIt = head.begin(); oIt != hEnd; ++oIt) {
					if (oIt != hIt) {
						if (getAtom(*oIt)->scc() == scc) { rule_.addHead(*oIt); }
						else                             { rule_.addToBody(*oIt, false); }
					}
				}
				RuleType t = simplifyRule(rule_, activeHead_, activeBody_);
				PrgBody* B = t != ENDRULE ? assignBodyFor(activeBody_, PrgEdge::NORMAL_EDGE, true) : 0;
				if (!B || B->value() == value_false) { continue; }
				if (t == BASICRULE) {
					++shifted;
					B->addHead(getAtom(activeHead_[0]), PrgEdge::NORMAL_EDGE);
				}
				else if (t == DISJUNCTIVERULE) {
					PrgDisj* x = getDisjFor(activeHead_, 0);
					B->addHead(x, PrgEdge::NORMAL_EDGE);
					x->assignVar(*this, *x->supps_begin());
					x->setInUpper(true);
					x->markSeen(true);
					++added;
					if ((sccMap[scc] & is_scc_non_hcf) == 0) {
						sccMap[scc] |= is_scc_non_hcf;
						nonHcfs_.add(scc);
					}
					if (!options().noGamma) {
						rule_.setType(BASICRULE);
						for (uint32 i = 1; i != rule_.heads.size(); ++i) { rule_.addToBody(rule_.heads[i], false); }
						rule_.heads.resize(1);
						WeightLitVec::iterator bIt = rule_.body.end();
						for (uint32 i = x->size();;) {
							t = simplifyRule(rule_, activeHead_, activeBody_);
							B = t != ENDRULE ? assignBodyFor(activeBody_, PrgEdge::GAMMA_EDGE, true) : 0;
							if (B && B->value() != value_false) { B->addHead(getAtom(activeHead_[0]), PrgEdge::GAMMA_EDGE); ++stats.gammas; }
							if (--i == 0) { break; }
							Var h          = rule_.heads[0];
							rule_.heads[0] = (--bIt)->first.var();
							*bIt           = WeightLiteral(negLit(h), 1);
						}
					}
				}
			}
		}
	}
	stats.rules(DISJUNCTIVERULE).second = added;
	stats.rules(BASICRULE).second      += shifted;
	stats.nonHcfs = (uint32)nonHcfs_.size();
	frozen_       = true;
}

// HACK: transform extended rules in non-hcf components
// REMOVE once native support for extended rules is implemented
void ProgramBuilder::prepareNonHcfComponents() {
	if (!disjunctions_.empty()) {
		BodyList ext;
		EdgeVec  heads;
		for (BodyList::const_iterator it = bodies_.begin(), end = bodies_.end(); it != end; ++it) {
			if ((*it)->type() != BodyInfo::NORMAL_BODY && (*it)->hasVar() && (*it)->value() != value_false) {
				uint32 scc = (*it)->scc(*this);
				if (nonHcfs_.find(scc)) {
					ext.push_back(*it);
				}
			}
		}
		if (ext.empty()) { return; }
		struct Tr : public PrgRuleTransform::ProgramAdapter {
			Tr(ProgramBuilder* x) : self(x), scc(0) {}
			Var newAtom() {
				Var x      = self->newAtom();
				PrgAtom* a = self->getAtom(x);
				self->sccAtoms_.push_back(a);
				a->setScc(scc);
				a->markSeen(true);
				atoms.push_back(x);
				return x;
			}
			void addRule(PrgRule& nr) {
				if (self->simplifyRule(nr, self->activeHead_, self->activeBody_) != ENDRULE) {
					PrgBody* B = self->assignBodyFor(self->activeBody_, PrgEdge::NORMAL_EDGE, false);
					if (B->value() != value_false) {
						B->addHead(self->getAtom(self->activeHead_[0]), PrgEdge::NORMAL_EDGE);
					}
				}
			}
			ProgramBuilder* self;
			uint32 scc;
			VarVec atoms;
		} tr(this);
		PrgRuleTransform trans;
		frozen_ = false;
		if (incData_) { incData_->startAux_ = (uint32)atoms_.size(); }
		for (BodyList::const_iterator it = ext.begin(), end = ext.end(); it != end; ++it) {
			uint32 scc = (*it)->scc(*this);
			rule_.clear();
			rule_.setType((*it)->type() == BodyInfo::COUNT_BODY ? CONSTRAINTRULE : WEIGHTRULE);
			rule_.setBound((*it)->bound());
			tr.scc = scc;
			for (uint32 i = 0; i != (*it)->size(); ++i) {
				rule_.addToBody((*it)->goal(i).var(), (*it)->goal(i).sign() == false, (*it)->weight(i));
			}
			heads.assign((*it)->heads_begin(), (*it)->heads_end());
			for (EdgeVec::const_iterator hIt = heads.begin(); hIt != heads.end(); ++hIt) {
				assert(hIt->isAtom());
				if (getAtom(hIt->node())->scc() == scc) {
					(*it)->removeHead(getAtom(hIt->node()), hIt->type());
					rule_.heads.assign(1, hIt->node());
					if (simplifyRule(rule_, activeHead_, activeBody_) != ENDRULE) {
						trans.transform(tr, rule_);	
					}
				}
			}
		}
		incTrAux(tr.atoms.size());
		while (!tr.atoms.empty()) {
			PrgAtom* ax = getAtom(tr.atoms.back());
			tr.atoms.pop_back();
			if (ax->supports()) {
				ax->setInUpper(true);
				ax->assignVar(*this, *ax->supps_begin());
			}
			else { assignValue(ax, value_false); }
		}
		frozen_ = true;
	}
}

// add (completion) nogoods
bool ProgramBuilder::addConstraints() {
	ClauseCreator gc(ctx_->master());
	if (options().iters == 0) {
		gc.addFlags(ClauseCreator::clause_force_simplify);
	}
	ctx_->startAddConstraints();
	ctx_->symTab().endInit();
	for (BodyList::const_iterator it = bodies_.begin(); it != bodies_.end(); ++it) {
		if ( !toConstraint((*it), *this, gc) ) { return false; }
	}
	const bool freezeAtoms = incData_ && ctx_->satPrepro.get() != 0;
	uint32 start           = startAtom();
	uint32 stop            = static_cast<uint32>(atoms_.size());
	// temporarily add unfreeze atoms so that completiton is executed
	for (VarIter it = unfreeze_begin(), end = unfreeze_end(); it != end; ++it) {
		atoms_.push_back(getAtom(*it));
	}
	check_precondition(ctx_->symTab().curBegin() == ctx_->symTab().end() || start <= ctx_->symTab().curBegin()->first,
		std::logic_error);
	SymbolTable::const_iterator sym = ctx_->symTab().lower_bound(ctx_->symTab().curBegin(), start);
	bool ok = true;
	for (AtomList::const_iterator it = atoms_.begin()+start; it != atoms_.end(); ++it) {
		if ( !toConstraint((*it), *this, gc) ) { ok = false; break; ; }
		Var id = uint32(it-atoms_.begin());
		if (sym != ctx_->symTab().end() && id == sym->first) {
			sym->second.lit = atoms_[getEqAtom(id)]->literal();
			++sym;
		}
		if (freezeAtoms && (*it)->hasVar()) {
			ctx_->setFrozen((*it)->var(), true);
		}
	}
	atoms_.resize(stop);
	if (ok && !sccAtoms_.empty()) {
		if (ctx_->sccGraph.get() == 0) {
			ctx_->sccGraph = new SharedDependencyGraph(nonHcfCfg_);
		}
		uint32 oldNodes = ctx_->sccGraph->nodes();
		ctx_->sccGraph->addSccs(*this, sccAtoms_, nonHcfs_);
		stats.ufsNodes  = ctx_->sccGraph->nodes()-oldNodes;
		sccAtoms_.clear();
	}
	return ok;
}
/////////////////////////////////////////////////////////////////////////////////////////
// misc/helper functions
/////////////////////////////////////////////////////////////////////////////////////////
PrgAtom* ProgramBuilder::resize(Var atomId) {
	while (atoms_.size() <= AtomList::size_type(atomId)) {
		newAtom();
	}
	return atoms_[getEqAtom(atomId)];
}

bool ProgramBuilder::propagate(bool backprop) {
	assert(frozen_);
	bool oldB = eqOpts_.backprop;
	eqOpts_.backprop = backprop;
	for (VarVec::size_type i = 0; i != propQ_.size(); ++i) {
		PrgAtom* a = getAtom(propQ_[i]);
		if (!a->propagateValue(*this, backprop)) {
			setConflict();
			return false;
		}
	}
	eqOpts_.backprop = oldB;
	propQ_.clear();
	return true;
}

// Simplifies the rule's body:
//   - removes duplicate literals: {a,b,a} -> {a, b}.
//   - checks for contradictions : {a, not a}
//   - removes literals with weight 0    : LB [a = 0, b = 2, c = 0, ...] -> LB [b = 2, ...]
//   - reduces weights > bound() to bound:  2 [a=1, b=3] -> 2 [a=1, b=2]
//   - merges duplicate literals         : LB [a=w1, b=w2, a=w3] -> LB [a=w1+w3, b=w2]
//   - checks for contradiction, i.e.
//     rule body contains both p and not p and both are needed
//   - replaces weight constraint with cardinality constraint 
//     if all body weights are equal
//   - replaces weight/cardinality constraint with normal body 
//     if sumW - minW < bound()
RuleType ProgramBuilder::simplifyBody(const PrgRule& r, BodyInfo& info) {
	info.reset();
	WeightLitVec& sBody= info.lits;
	if (r.bodyHasBound() && r.bound() <= 0) {
		return BASICRULE;
	}
	sBody.reserve(r.body.size());
	RuleType resType = r.type();
	weight_t w       = 0;
	weight_t BOUND   = r.bodyHasBound() ? r.bound() : std::numeric_limits<weight_t>::max();
	weight_t trueW   = 0;
	uint32   pos     = 0;
	uint32   hash    = 0;
	bool     dirty   = r.bodyHasWeights();
	Literal lit;
	for (WeightLitVec::const_iterator it = r.body.begin(), bEnd = r.body.end(); it != bEnd; ++it) {
		if (it->second == 0) continue; // skip irrelevant lits
		check_precondition(it->second>0, std::logic_error);
		PrgAtom* a = resize(it->first.var());
		lit        = Literal(a->id(), it->first.sign());// replace any eq atoms
		w          = std::min(it->second, BOUND);       // reduce weights to bound
		if (a->value() != value_free || !a->relevant()) {
			bool vSign = a->value() == value_false || !a->relevant();
			if (vSign != lit.sign()) {
				// literal is false - drop rule?
				if (r.bodyIsSet()) { resType = ENDRULE; break; }
				continue;
			}
			else if (a->value() != value_weak_true) {
				// literal is true - drop from rule
				trueW += w;
				continue;
			}
		}
		if (!ruleState_.inBody(lit)) {  // literal not seen yet
			ruleState_.addToBody(lit);    // add to simplified body
			sBody.push_back(WeightLiteral(lit, w));
			pos += !lit.sign();
			hash+= !lit.sign() ? hashId(lit.var()) : hashId(-lit.var());
		}
		else if (!r.bodyIsSet()) {      // Merge duplicate lits
			WeightLiteral& oldLit = info[info.findLit(lit)];
			weight_t       oldW   = oldLit.second;
			assert((INT_MAX-oldW)>= w && "Integer overflow!");
			w             = std::min(oldW + w, BOUND); // remember new weight
			oldLit.second = w;
			dirty         = true;
			if (resType == CONSTRAINTRULE) resType = WEIGHTRULE;
		}
		dirty |= ruleState_.inBody(~lit);
	}
	weight_t minW    = 1;
	weight_t maxW    = 1;
	weight_t realSum = (weight_t)sBody.size();
	weight_t sumW    = (weight_t)sBody.size();
	if (dirty) {
		minW    = std::numeric_limits<weight_t>::max();
		realSum = sumW = 0;
		for (WeightLitVec::size_type i = 0; i != sBody.size(); ++i) {
			lit = sBody[i].first; w = sBody[i].second;
			minW = std::min(minW, w);
			maxW = std::max(maxW, w);
			sumW+= w;
			if      (!ruleState_.inBody(~lit)) { realSum += w; }
			else if (r.bodyIsSet())            { resType = ENDRULE; break; }
			else if (lit.sign())               { 
				// body contains lit and ~lit: we can achieve at most max(weight(lit), weight(~lit))
				realSum += std::max(w, info[info.findLit(~lit)].second);
			}
		}
	}
	weight_t bound = r.bodyHasBound() ? r.bound() - trueW : static_cast<weight_t>(sBody.size());
	if (resType != ENDRULE && r.bodyHasBound()) {
		if      (bound <= 0)            { sBody.clear(); resType = BASICRULE; bound = 0; }
		else if (realSum < bound)       { resType = ENDRULE; }
		else if ((sumW - minW) < bound) { resType = BASICRULE;      bound = (weight_t)sBody.size(); }
		else if (minW == maxW)          { resType = CONSTRAINTRULE; bound = (bound+(minW-1))/minW;  }
	}
	info.init(resType, bound, hash, pos);
	return resType;
}

RuleType ProgramBuilder::simplifyRule(const PrgRule& r, VarVec& head, BodyInfo& body) {
	RuleType type = simplifyBody(r, body);
	head.clear();
	if (type != ENDRULE && type != OPTIMIZERULE) {
		bool blocked = false, taut = false;
		weight_t sum = -1;
		for (VarVec::const_iterator it = r.heads.begin(), end = r.heads.end(); it != end; ++it) {
			if (!ruleState_.isSet(*it, RuleState::any_flag)) {
				head.push_back(*it);
				ruleState_.addToHead(*it);
			}
			else if (!ruleState_.isSet(*it, RuleState::head_flag)) {
				weight_t wPos = ruleState_.inBody(posLit(*it)) ? body.weight(posLit(*it)) : 0;
				weight_t wNeg = ruleState_.inBody(negLit(*it)) ? body.weight(negLit(*it)) : 0;
				if (sum == -1) sum = body.sum();
				if ((sum - wPos) < body.bound()) {
					taut    = (type != CHOICERULE);
				}
				else if ((sum - wNeg) < body.bound()) {
					blocked = (type != CHOICERULE);
				}
				else {
					head.push_back(*it);
					ruleState_.addToHead(*it);
				}
			}
		}
		for (VarVec::const_iterator it = head.begin(), end = head.end(); it != end; ++it) {
			ruleState_.clear(*it);
		}
		if (blocked && type != DISJUNCTIVERULE) {
			head.clear();
			head.push_back(0);
		}
		else if (taut && (type == DISJUNCTIVERULE || head.empty())) {
			head.clear();
			type = ENDRULE;
		}
		else if (type == DISJUNCTIVERULE && head.size() == 1) {
			type = BASICRULE;
		}
		else if (head.empty()) {
			type = ENDRULE;
		}
	}
	for (WeightLitVec::size_type i = 0; i != body.size(); ++i) {
		ruleState_.clear(body[i].first.var());
	}
	return type;
}

// create new atom aux representing supports, i.e.
// aux == S1 v ... v Sn
Literal ProgramBuilder::getEqAtomLit(Literal lit, const BodyList& supports, Preprocessor& p, const SccMap& sccMap) {
	if (supports.empty() || lit == negLit(0)) { return negLit(0); }
	if (supports.size() == 1 && supports[0]->size() < 2) { 
		return supports[0]->size() == 0 ? posLit(0) : supports[0]->goal(0); 
	}
	if (p.getRootAtom(lit) != varMax)         { return posLit(p.getRootAtom(lit)); }
	incTrAux(1);
	Var auxV     = newAtom();
	PrgAtom* aux = getAtom(auxV);
	uint32 scc   = PrgNode::noScc;
	aux->setLiteral(lit);
	aux->markSeen(true);
	p.setRootAtom(aux->literal(), auxV);
	for (BodyList::const_iterator sIt = supports.begin(); sIt != supports.end(); ++sIt) {
		PrgBody* b = *sIt;
		if (b->relevant() && b->value() != value_false) {
			for (uint32 g = 0; scc == PrgNode::noScc && g != b->size() && !b->goal(g).sign(); ++g) {
				uint32 aScc = getAtom(b->goal(g).var())->scc();
				if (aScc != PrgNode::noScc && (sccMap[aScc] & 1u)) { scc = aScc; }
			}
			b->addHead(aux, PrgEdge::NORMAL_EDGE);
			if (b->value() != aux->value()) { assignValue(aux, b->value()); }
			aux->setInUpper(true);
		}
	}
	if (!aux->inUpper()) {
		aux->setValue(value_false);
		return negLit(0);
	}
	else if (scc != PrgNode::noScc) {
		aux->setScc(scc);
		sccAtoms_.push_back(aux);
	}
	return posLit(auxV);
}

PrgBody* ProgramBuilder::getBodyFor(BodyInfo& body, bool addDeps) {
	uint32 bodyId = equalBody(bodyIndex_.equal_range(body.hash), body);
	if (bodyId != varMax) {
		return getBody(bodyId);
	}
	// no corresponding body exists, create a new object
	bodyId        = (uint32)bodies_.size();
	PrgBody* b    = PrgBody::create(*this, bodyId, body, addDeps);
	bodyIndex_.insert(BodyIndex::value_type(body.hash, bodyId));
	bodies_.push_back(b);
	if (b->isSupported()) {
		initialSupp_.push_back(bodyId);
	}
	return b;
}

PrgBody* ProgramBuilder::assignBodyFor(BodyInfo& body, EdgeType depEdge, bool simpStrong) {
	PrgBody* b = getBodyFor(body, depEdge != PrgEdge::GAMMA_EDGE);
	if (!b->hasVar() && !b->seen()) {
		uint32 eqId;
		b->markDirty();
		b->simplify(*this, simpStrong, &eqId);
		if (eqId != b->id()) {
			assert(b->id() == bodies_.size()-1);
			removeBody(b, body.hash);
			bodies_.pop_back();
			if (depEdge != PrgEdge::GAMMA_EDGE) {
				for (uint32 i = 0; i != b->size(); ++i) {
					getAtom(b->goal(i).var())->removeDep(b->id(), !b->goal(i).sign());
				}
			}
			b->destroy();
			b = bodies_[eqId];
		}
	}
	b->markSeen(true);
	b->assignVar(*this);
	return b;
}

uint32 ProgramBuilder::equalBody(const IndexRange& range, BodyInfo& body) const {
	bool sorted = false;
	for (IndexIter it = range.first; it != range.second; ++it) {
		PrgBody& o = *bodies_[it->second];
		if (o.type() == body.type() && o.size() == body.size() && o.bound() == body.bound() && (body.posSize() == 0u || o.goal(body.posSize()-1).sign() == false)) {
			// bodies are structurally equivalent - check if they contain the same literals
			if ((o.relevant() || (o.eq() && getBody(o.id())->relevant())) && o.eqLits(body.lits, sorted)) {
				assert(o.id() == it->second || o.eq());
				return o.id();
			}
		}
	}
	return varMax;
}
uint32 ProgramBuilder::findEqBody(PrgBody* b, uint32 hash) {
	ProgramBuilder::IndexRange eqRange = bodyIndex_.equal_range(hash);
	// check for existing body
	if (eqRange.first != eqRange.second) {
		activeBody_.reset();
		WeightLitVec& lits = activeBody_.lits;
		uint32 p = 0;
		for (uint32 i = 0, end = b->size(); i != end; ++i) { 
			lits.push_back(WeightLiteral(b->goal(i), b->weight(i))); 
			p += !lits.back().first.sign(); 
		}
		activeBody_.init(b->type(), b->bound(), hash, p);
		return equalBody(eqRange, activeBody_);
	}
	return varMax;
}

PrgDisj* ProgramBuilder::getDisjFor(const VarVec& heads, uint32 headHash) {
	PrgDisj* d = 0;
	if (headHash) {
		ProgramBuilder::IndexRange eqRange = disjIndex_.equal_range(headHash);
		for (; eqRange.first != eqRange.second; ++eqRange.first) {
			PrgDisj& o = *disjunctions_[eqRange.first->second];
			if (o.relevant() && o.size() == heads.size() && ruleState_.allMarked(heads, RuleState::head_flag)) {
				assert(o.id() == eqRange.first->second);
				d = &o;
				break;
			}
		}
		for (VarVec::const_iterator it = heads.begin(), end = heads.end(); it != end; ++it) {
			ruleState_.clear(*it);
		}
	}
	if (!d) {
		// no corresponding disjunction exists, create a new object
		// and link it to all atoms
		uint32 id    = disjunctions_.size();
		d            = PrgDisj::create(id, heads);
		disjunctions_.push_back(d);
		PrgEdge edge = PrgEdge::newEdge(id, PrgEdge::CHOICE_EDGE, PrgEdge::DISJ_NODE);
		for (VarVec::const_iterator it = heads.begin(), end = heads.end(); it != end; ++it) {
			getAtom(*it)->addSupport(edge);
		}
		if (headHash) {
			disjIndex_.insert(DisjIndex::value_type(headHash, d->id()));
		}
	}
	return d;
}

// body has changed - update index
uint32 ProgramBuilder::update(PrgBody* body, uint32 oldHash, uint32 newHash) {
	uint32 id   = removeBody(body, oldHash);
	if (body->relevant()) {
		uint32 eqId = findEqBody(body, newHash);
		if (eqId == varMax) {
			// No equivalent body found. 
			// Add new entry to index
			bodyIndex_.insert(BodyIndex::value_type(newHash, id));
		}
		return eqId;
	}
	return varMax;
}

// body b has changed - remove old entry from body node index
uint32 ProgramBuilder::removeBody(PrgBody* b, uint32 hash) {
	IndexRange ra = bodyIndex_.equal_range(hash);
	uint32 id     = b->id();
	for (; ra.first != ra.second; ++ra.first) {
		if (bodies_[ra.first->second] == b) {
			id = ra.first->second;
			bodyIndex_.erase(ra.first);
			break;
		}
	}
	return id;
}

PrgAtom* ProgramBuilder::mergeEqAtoms(PrgAtom* a, Var rootId) {
	rootId        = getEqAtom(rootId);
	PrgAtom* root = getAtom(rootId);
	assert(!a->eq() && !root->eq());
	if (a->ignoreScc())       { root->setIgnoreScc(true); }
	if (a->frozen())          { root->setFrozen(true);    }
	if (!mergeValue(a, root)) { setConflict(); return 0;  }
	assert(a->value() == root->value() || (root->value() == value_true && a->value() == value_weak_true));
	a->setEq(rootId);
	incEqs(Var_t::atom_var);
	return root;
}

// returns whether posSize(root) <= posSize(body)
bool ProgramBuilder::positiveLoopSafe(PrgBody* body, PrgBody* root) const {
	uint32 i = 0, end = std::min(body->size(), root->size());
	while (i != end && body->goal(i).sign() == root->goal(i).sign()) { ++i; }
	return i == root->size() || root->goal(i).sign();	
}

PrgBody* ProgramBuilder::mergeEqBodies(PrgBody* b, Var rootId, bool hashEq, bool atomsAssigned) {
	rootId        = getEqNode(bodies_, rootId);
	PrgBody* root = getBody(rootId);
	bool     bp   = options().backprop;
	if (b == root) { return root; }
	assert(!b->eq() && !root->eq() && (hashEq || b->literal() == root->literal()));
	if (b->value() != root->value() && (!mergeValue(b, root) || !root->propagateValue(*this, bp) || !b->propagateValue(*this, bp))) {
		setConflict(); 
		return 0; 
	}
	assert(b->value() == root->value());
	if (hashEq || positiveLoopSafe(b, root)) {
		b->setLiteral(root->literal());
		if (!root->mergeHeads(*this, *b, atomsAssigned)) {
			setConflict(); 
			return 0;
		}
		incEqs(Var_t::body_var);
		b->setEq(rootId);
		return root;
	}
	return b;
}

uint32 ProgramBuilder::getFalseAtom() const {
	for (VarVec::size_type i = 1; i < atoms_.size(); ++i) {
		if (!atoms_[i]->eq() && atoms_[i]->value() == value_false) {
			return i;
		}
	}
	return 0;
}

VarVec& ProgramBuilder::getSupportedBodies(bool sorted) {
	if (sorted) {
		std::stable_sort(initialSupp_.begin(), initialSupp_.end(), LessBodySize(bodies_));
	}
	return initialSupp_;
}

void ProgramBuilder::transform(const PrgBody& body, BodyInfo& out) const {
	out.reset();
	out.lits.reserve(body.size());
	uint32 p = 0, end = body.size();
	while (p != end && !body.goal(p).sign()) { ++p; }
	uint32 R[2][2] = { {p, end}, {0, p} };
	for (uint32 range = 0; range != 2; ++range) {
		for (uint32 x = R[range][0]; x != R[range][1]; ++x) {
			WeightLiteral wl(body.goal(x), body.weight(x));
			if (getAtom(wl.first.var())->hasVar()) {
				out.lits.push_back(wl);
			}
		}
	}
	out.init(body.type(), body.bound(), 0, p);
}

void ProgramBuilder::transform(const MinimizeRule& body, BodyInfo& out) const {
	out.reset();
	uint32 pos = 0;
	for (WeightLitVec::const_iterator it = body.lits_.begin(), end = body.lits_.end(); it != end; ++it) {
		if (it->first.sign() && getAtom(it->first.var())->hasVar()) {
			out.lits.push_back(*it);
		}
	}
	for (WeightLitVec::const_iterator it = body.lits_.begin(), end = body.lits_.end(); it != end; ++it) {
		if (!it->first.sign() && getAtom(it->first.var())->hasVar()) {
			out.lits.push_back(*it);
			++pos;
		}
	}
	out.init(BodyInfo::SUM_BODY, -1, 0, pos);
}

void ProgramBuilder::writeBody(const BodyInfo& body, std::ostream& out) const {
	if (body.type() == BodyInfo::SUM_BODY && body.bound() != -1) { out << body.bound() << " "; }
	out << body.size() << " ";
	out << (body.size() - body.posSize()) << " ";
	if (body.type() == BodyInfo::COUNT_BODY) { out << body.bound() << " "; }
	for (WeightLitVec::const_iterator it = body.lits.begin(), end = body.lits.end(); it != end; ++it) {
		out << it->first.var() << " ";
	}
	if (body.type() == BodyInfo::SUM_BODY) {
		for (WeightLitVec::const_iterator it = body.lits.begin(), end = body.lits.end(); it != end; ++it) {
			out << it->second << " ";
		}
	}
}

}
