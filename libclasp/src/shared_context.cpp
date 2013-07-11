// 
// Copyright (c) 2010-2012, Benjamin Kaufmann
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
#include <clasp/shared_context.h>
#include <clasp/solver.h>
#include <clasp/clause.h>
#include <clasp/enumerator.h>
#include <clasp/dependency_graph.h>
#if WITH_THREADS
#include <clasp/util/thread.h>
#endif
namespace Clasp {
/////////////////////////////////////////////////////////////////////////////////////////
// ShortImplicationsGraph::ImplicationList
/////////////////////////////////////////////////////////////////////////////////////////
#if WITH_THREADS
ShortImplicationsGraph::Block::Block() {
	for (int i = 0; i != block_cap; ++i) { data[i] = posLit(0); }
	size_lock = 0;
	next      = 0;
}
void ShortImplicationsGraph::Block::addUnlock(uint32 lockedSize, const Literal* x, uint32 xs) {
	std::copy(x, x+xs, data+lockedSize);
	size_lock = ((lockedSize+xs) << 1);
}
bool ShortImplicationsGraph::Block::tryLock(uint32& size) {
	uint32 s = size_lock;
	if ((s & 1) == 0 && size_lock.compare_and_swap(s | 1, s) == s) {
		size = s >> 1;
		return true;
	}
	return false;
}

#define FOR_EACH_LEARNT(x, Y) \
	for (Block* b = (x).learnt; b ; b = b->next) \
		for (const Literal* Y = b->begin(), *endof = b->end(); Y != endof; Y += 2 - Y->watched())

	
ShortImplicationsGraph::ImplicationList::~ImplicationList() {
	clear(true);
}

void ShortImplicationsGraph::ImplicationList::clear(bool b) {
	ImpListBase::clear(b);
	for (Block* x = learnt; x; ) {
		Block* t = x;
		x = x->next;
		delete t;
	}
	learnt = 0;
}

void ShortImplicationsGraph::ImplicationList::addLearnt(Literal p, Literal q) {
	Literal nc[2] = {p, q};
	uint32  ns    = 1 + !isSentinel(q);
	if (ns == 1) { nc[0].watch(); }
	for (Block* x;;) {
		x = learnt;
		if (x) {
			uint32 lockedSize;
			if (x->tryLock(lockedSize)) {
				if ( (lockedSize + ns) <=  Block::block_cap ) {
					x->addUnlock(lockedSize, nc, ns);
				}
				else {
					Block* t = new Block();
					t->addUnlock(0, nc, ns);
					t->next   = x; // x is full and remains locked forever
					x = learnt= t; // publish new block - unlocks x and learnt
				}
				return;
			}
			else { 
				std::this_thread::yield();
			}
		}
		else {
			x = new Block();
			if (learnt.compare_and_swap(x, 0) != 0) {
				delete x;
			}
		}
	}
}

bool ShortImplicationsGraph::ImplicationList::hasLearnt(Literal q, Literal r) const {
	const bool binary = isSentinel(r);
	FOR_EACH_LEARNT(*this, imp) {
		if (imp[0] == q || imp[0] == r) {
			// binary clause subsumes new bin/tern clause
			if (imp->watched())                          { return true; }
			// existing ternary clause subsumes new tern clause
			if (!binary && (imp[1] == q || imp[1] == r)) { return true; }
		}
	}
	return false;
}

void ShortImplicationsGraph::ImplicationList::move(ImplicationList& other) {
	ImpListBase::move(other);
	delete learnt;
	learnt       = other.learnt;
	other.learnt = 0;
}
#undef FOR_EACH_LEARNT
#endif
/////////////////////////////////////////////////////////////////////////////////////////
// ShortImplicationsGraph
/////////////////////////////////////////////////////////////////////////////////////////
ShortImplicationsGraph::ShortImplicationsGraph() {
	bin_[0]  = bin_[1]  = 0;
	tern_[0] = tern_[1] = 0;
	shared_  = false;
}
ShortImplicationsGraph::~ShortImplicationsGraph() {
	PodVector<ImplicationList>::destruct(graph_);
}
void ShortImplicationsGraph::resize(uint32 nodes) {
	if (graph_.capacity() >= nodes) {
		graph_.resize(nodes);
	}
	else {
		ImpLists temp; temp.resize(nodes);
		for (ImpLists::size_type i = 0; i != graph_.size(); ++i) {
			temp[i].move(graph_[i]);
		}
		graph_.swap(temp);
	}
}

uint32 ShortImplicationsGraph::numEdges(Literal p) const { return graph_[p.index()].size(); }

bool ShortImplicationsGraph::addBinary(Literal p, Literal q, bool learnt) {
	uint32 added = 0;
	if (!shared_) {
		if (!learnt) { p.clearWatch(); q.clearWatch(); }
		else         { p.watch(); q.watch(); }
		getList(~p).push_left(q);
		getList(~q).push_left(p);
		added = 1;
	}
#if WITH_THREADS
	else if (learnt && !getList(~p).hasLearnt(q)) {
		getList(~p).addLearnt(q);
		getList(~q).addLearnt(p);
		added = 1;
	}
#endif
	bin_[learnt] += added;
	return added > 0;
}

bool ShortImplicationsGraph::addTernary(Literal p, Literal q, Literal r, bool learnt) {
	uint32 added = 0;
	if (!shared_) {
		if (!learnt) { p.clearWatch(); q.clearWatch(); r.clearWatch(); }
		else         { p.watch(); q.watch(); r.watch(); }
		getList(~p).push_right(std::make_pair(q, r));
		getList(~q).push_right(std::make_pair(p, r));
		getList(~r).push_right(std::make_pair(p, q));
		added = 1;
	}
#if WITH_THREADS
	else if (learnt && !getList(~p).hasLearnt(q, r)) {
		getList(~p).addLearnt(q, r);
		getList(~q).addLearnt(p, r);
		getList(~r).addLearnt(p, q);
		added = 1;
	}
#endif
	tern_[learnt] += added;
	return added > 0;
}

void ShortImplicationsGraph::remove_bin(ImplicationList& w, Literal p) {
	w.erase_left_unordered(std::find(w.left_begin(), w.left_end(), p)); 
	w.try_shrink(); 
}
void ShortImplicationsGraph::remove_tern(ImplicationList& w, Literal p) {
	w.erase_right_unordered(std::find_if(w.right_begin(), w.right_end(), PairContains<Literal>(p))); 
	w.try_shrink();	
}

void ShortImplicationsGraph::removeTrue(const Solver& s, Literal p) {
	typedef ImplicationList SWL;
	SWL& negPList = graph_[(~p).index()];
	SWL& pList    = graph_[ (p).index()];
	// remove every binary clause containing p -> clause is satisfied
	for (SWL::left_iterator it = negPList.left_begin(), end = negPList.left_end(); it != end; ++it) {
		--bin_[it->watched()];
		remove_bin(graph_[(~*it).index()], p);
	}
	// remove every ternary clause containing p -> clause is satisfied
	for (SWL::right_iterator it = negPList.right_begin(), end = negPList.right_end(); it != end; ++it) {
		--tern_[it->first.watched()];
		remove_tern(graph_[ (~it->first).index() ], p);
		remove_tern(graph_[ (~it->second).index() ], p);
	}
	// transform ternary clauses containing ~p to binary clause
	for (SWL::right_iterator it = pList.right_begin(), end = pList.right_end(); it != end; ++it) {
		Literal q = it->first;
		Literal r = it->second;
		--tern_[q.watched()];
		remove_tern(graph_[(~q).index()], ~p);
		remove_tern(graph_[(~r).index()], ~p);
		if (s.value(q.var()) == value_free && s.value(r.var()) == value_free) {
			// clause is binary on dl 0
			addBinary(q, r, false);
		}
		// else: clause is SAT and removed when the satisfied literal is processed
	}
	graph_[(~p).index()].clear(true);
	graph_[ (p).index()].clear(true);
}
struct ShortImplicationsGraph::Propagate {
	Propagate(Solver& a_s) : s(&a_s) {}
	bool unary(Literal p, Literal x) const { return s->isTrue(x) || s->force(x, Antecedent(p)); }
	bool binary(Literal p, Literal x, Literal y) const {
		ValueRep vx = s->value(x.var()), vy;
		if (vx != trueValue(x) && (vy=s->value(y.var())) != trueValue(y) && vx + vy) {
			return vx != 0 ? s->force(y, Antecedent(p, ~x)) : s->force(x, Antecedent(p, ~y));
		}
		return true;
	}
	Solver* s;
};
struct ShortImplicationsGraph::ReverseArc {
	ReverseArc(const Solver& a_s, uint32 m, Antecedent& o) : s(&a_s), out(&o), maxL(m) {}
	bool unary(Literal, Literal x) const {
		if (!isRevLit(*s, x, maxL)) { return true; }
		*out = Antecedent(~x); 
		return false;
	}
	bool binary(Literal, Literal x, Literal y) const {
		if (!isRevLit(*s, x, maxL) || !isRevLit(*s, y, maxL)) { return true; }
		*out = Antecedent(~x, ~y);
		return false;
	}
	const Solver* s; Antecedent* out; uint32 maxL;
};
bool ShortImplicationsGraph::propagate(Solver& s, Literal p) const { return forEach(p, Propagate(s)); }
bool ShortImplicationsGraph::reverseArc(const Solver& s, Literal p, uint32 maxLev, Antecedent& out) const { return !forEach(p, ReverseArc(s, maxLev, out)); }
bool ShortImplicationsGraph::propagateBin(Assignment& out, Literal p, uint32 level) const {
	const ImplicationList& x = graph_[p.index()];
	Antecedent ante(p);
	for (ImplicationList::const_left_iterator it = x.left_begin(), end = x.left_end(); it != end; ++it) {
		if (!out.assign(*it, level, p)) { return false; }
	}
	return true;
}
/////////////////////////////////////////////////////////////////////////////////////////
// SatPreprocessor
/////////////////////////////////////////////////////////////////////////////////////////
SatPreprocessor::~SatPreprocessor() {
	for (ClauseList::size_type i = 0; i != clauses_.size(); ++i) {
		if (clauses_[i]) { clauses_[i]->destroy(); }
	}
	ClauseList().swap(clauses_);
	for (Clause* r = elimTop_; r;) {
		Clause* t = r;
		 r = r->next();
		 t->destroy();
	}
	elimTop_ = 0;
}
void SatPreprocessor::doCleanUp() {
	for (ClauseList::size_type i = 0; i != clauses_.size(); ++i) {
		if (clauses_[i]) { clauses_[i]->destroy(); }
	}
	ClauseList().swap(clauses_);
}
void SatPreprocessor::cleanUp() {
	doCleanUp();                  // <- virtual dispatch never calls a pure virtual
	SatPreprocessor::doCleanUp(); // <- call our version
}
void SatPreprocessor::reportProgress(const PreprocessEvent& e) { ctx_->reportProgress(e); }
bool SatPreprocessor::addClause(const LitVec& cl) {
	if (cl.size() > 1) {
		clauses_.push_back( Clause::newClause( cl ) );
	}
	else if (cl.size() == 1) {
		units_.push_back(cl[0]);
	}
	else {
		return false;
	}
	return true;
}	
bool SatPreprocessor::preprocess(SharedContext& ctx, bool enumerateModels) {
	ctx_ = &ctx;
	Solver* s = ctx_->master();
	struct OnExit {
		SatPreprocessor* self;
		~OnExit() { if (self) self->cleanUp(); }
	} onExit; onExit.self = this;
	ConstraintType unit = Constraint_t::static_constraint;
	for (LitVec::const_iterator it = units_.begin(), end = units_.end(); it != end; ++it) {
		if (!s->addUnary(*it, unit)) return false;
	}
	units_.clear();
	// skip preprocessing if other constraints are UNSAT
	if (!s->propagate()) return false;
	// 1. remove SAT-clauses, strengthen clauses w.r.t false literals, attach
	if (initPreprocess(enumerateModels)) {
		ClauseList::size_type j = 0; 
		for (ClauseList::size_type i = 0; i != clauses_.size(); ++i) {
			Clause* c   = clauses_[i]; assert(c);
			clauses_[i] = 0;
			c->simplify(*s);
			if (s->value((*c)[0].var()) == value_free) {
				clauses_[j++] = c; 
			}
			else if (c) {
				Literal x = (*c)[0];
				c->destroy(); 
				if (!s->addUnary(x, unit)) { return false; }
			}
		}
		clauses_.erase(clauses_.begin()+j, clauses_.end());
		// 2. run preprocessing
		if (!s->propagate() || !doPreprocess()) return false;
	}
	// simplify other constraints w.r.t any newly derived top-level facts
	if (!s->simplify()) return false;
	// 3. move preprocessed clauses to ctx
	ClauseCreator nc(s);
	for (ClauseList::size_type i = 0; i != clauses_.size(); ++i) {
		if (clauses_[i]) {
			Clause& c = *clauses_[i];
			nc.start();
			for (uint32 x = 0; x != c.size(); ++x) {  nc.add(c[x]);  }
			c.destroy();
			nc.end();
		}
	}
	ClauseList().swap(clauses_);
	return true;
}
void SatPreprocessor::extendModel(Assignment& m, LitVec& open) {
	if (!open.empty()) {
		// flip last unconstraint variable to get "next" model
		open.back() = ~open.back();
	}
	doExtendModel(m, open);
	// remove unconstraint vars already flipped
	while (!open.empty() && open.back().sign()) {
		open.pop_back();
	}
}
SatPreprocessor::Clause* SatPreprocessor::Clause::newClause(const LitVec& lits) {
	void* mem = ::operator new( sizeof(Clause) + (lits.size()-1)*sizeof(Literal) );
	return new (mem) Clause(lits);
}
SatPreprocessor::Clause::Clause(const LitVec& lits) : size_((uint32)lits.size()), inQ_(0), marked_(0) {
	std::memcpy(lits_, &lits[0], lits.size()*sizeof(Literal));
}
void SatPreprocessor::Clause::strengthen(Literal p) {
	uint64 a = 0;
	uint32 i, end;
	for (i   = 0; lits_[i] != p; ++i) { a |= Clause::abstractLit(lits_[i]); }
	for (end = size_-1; i < end; ++i) { lits_[i] = lits_[i+1]; a |= Clause::abstractLit(lits_[i]); }
	--size_;
	data_.abstr = a;
}
void SatPreprocessor::Clause::simplify(Solver& s) {
	uint32 i;
	for (i = 0; i != size_ && s.value(lits_[i].var()) == value_free; ++i) {;}
	if      (i == size_)        { return; }
	else if (s.isTrue(lits_[i])){ std::swap(lits_[i], lits_[0]); return;  }
	uint32 j = i++;
	for (; i != size_; ++i) {
		if (s.isTrue(lits_[i]))   { std::swap(lits_[i], lits_[0]); return;  }
		if (!s.isFalse(lits_[i])) { lits_[j++] = lits_[i]; }
	}
	size_ = j;
}
void SatPreprocessor::Clause::destroy() {
	void* mem = this;
	this->~Clause();
	::operator delete(mem);
}
/////////////////////////////////////////////////////////////////////////////////////////
// SharedContext
/////////////////////////////////////////////////////////////////////////////////////////
static DefaultConfiguration config_def_s;
SharedContext::SharedContext(const ContextOptions& opts) 
	: solve(0), symTabPtr_(new SharedSymTab()), accu_(0), progress_(0), lastInit_(0), lastTopLevel_(0), opts_(opts) {
	Antecedent::checkPlatformAssumptions();
	init(&config_def_s);
	
}

SharedContext::SharedContext(const SharedContext& rhs,  InitMode) 
	: solve(0), accu_(0), progress_(0), lastInit_(0), lastTopLevel_(0)
	, opts_(rhs.opts_), share_(rhs.share_) {
	share_.count = 1;
	symTabPtr_   = rhs.symTabPtr_;
	++symTabPtr_->refs;
	init(rhs.configuration());
}

void SharedContext::init(Configuration* c) {
	Var sentinel  = addVar(Var_t::atom_var); // sentinel always present
	setFrozen(sentinel, true);
	problem_.vars = 0;
	config_       = c;
	config_.release();
	addSolver();
	addEnumerator(0);
}

void SharedContext::enableStats(uint32 lev) {
	if (lev) {
		SolveStats& stats = master()->stats;
		if (!accu_)                                  { accu_ = new SolveStats(); }
		else                                         { accu_->reset(); }
		if (!stats.extra)                            { stats.enableExtended();}
		if (lev > 1 && !stats.jumps)                 { stats.enableJump();    }
		if (sccGraph.get() && sccGraph->numNonHcfs()){ sccGraph->enableStats(lev); }
		for (uint32 i = 0; i != solvers_.size(); ++i){ solvers_[i]->stats.reset(); }
		accu_->enableStats(stats);
	}
}

SolveStats* SharedContext::accuStats(bool accu) const {
	if (accu_ && accu) {
		for (uint32 i = 0; i != solvers_.size(); ++i) {
			accu_->accu(solvers_[i]->stats);
		}
	}
	return accu_;
}
void SharedContext::accuStats(const SolveStats& s) const {
	if (accu_) { accu_->accu(s); }
}

void SharedContext::copyVars(SharedContext& other) {
	problem_.vars            = other.problem_.vars;
	problem_.vars_eliminated = other.problem_.vars_eliminated;
	problem_.vars_frozen     = other.problem_.vars_frozen;
	varInfo_                 = other.varInfo_;
	if (&symTab() != &other.symTab()) {
		other.symTab().copyTo(symTab());
	}
}

SharedContext::~SharedContext() {
	while (!solvers_.empty()) { delete solvers_.back(); solvers_.pop_back(); }
	delete accu_;
	if (--symTabPtr_->refs == 0) delete symTabPtr_;
}

void SharedContext::reset() {
	this->~SharedContext();
	new (this) SharedContext();	
}

void SharedContext::concurrency(uint32 n) {
	if (n <= 1) { share_.count = 1; opts_.shareMode = 0; }
	else        { share_.count = n; solvers_.reserve(n); }
	while (solvers_.size() > share_.count) {
		delete solvers_.back(); 
		solvers_.pop_back();
	}
}

Solver& SharedContext::addSolver() {
	uint32 id    = (uint32)solvers_.size();
	share_.count = std::max(share_.count, id + 1);
	Solver* s    = new Solver(this, id);
	solvers_.push_back(s);
	return *s;
}

void SharedContext::setConfiguration(Configuration* c, bool own) {
	if (config_.get() != c) {
		if (c == 0) { c = &config_def_s; own = false; }
		config_ = c;
		if (!own) config_.release();
		config_->init(*this);
	}
	else if (own != config_.is_owner()) {
		if (own) config_.acquire();
		else     config_.release();
	}
}

void SharedContext::reserveVars(uint32 varGuess) {
	varInfo_.reserve(varGuess + 1);
}

Var SharedContext::addVar(VarType t, bool eq) {
	VarInfo nv;
	if (t == Var_t::body_var) { nv.set(VarInfo::BODY); }
	if (eq)                   { nv.set(VarInfo::EQ);   }
	varInfo_.push_back(nv);
	++problem_.vars;
	return numVars();
}

void SharedContext::requestTagLiteral() {
	if (tag_ == posLit(0)) {
		tag_ = negLit(0);
	}
}

void SharedContext::removeTagLiteral() {
	assert(master());
	if (!isSentinel(tag_)) {
		master()->force(tag_, 0);
	}
	tag_ = posLit(0);
}

void SharedContext::requestData(Var v) {
	master()->assign_.requestData(v + 1);
}

void SharedContext::setFrozen(Var v, bool b) {
	assert(validVar(v)); 
	if (v && b != varInfo_[v].has(VarInfo::FROZEN)) {
		varInfo_[v].toggle(VarInfo::FROZEN);
		b ? ++problem_.vars_frozen : --problem_.vars_frozen;
	}
}
bool SharedContext::eliminated(Var v) const {
	assert(validVar(v)); 
	return !master()->assign_.valid(v);
}

void SharedContext::eliminate(Var v) {
	assert(validVar(v) && !frozen() && master()->decisionLevel() == 0); 
	if (!eliminated(v)) {
		++problem_.vars_eliminated;
		// eliminate var from assignment - no longer a decision variable!
		master()->assign_.eliminate(v);
	}
}

Solver& SharedContext::startAddConstraints(uint32 constraintGuess) {
	share_.frozen = 0;
	share_.winner = 0;
	if (tag_ == negLit(0)) {
		// add aux var for tag literal
		tag_ = posLit(addVar(Var_t::atom_var));
		setFrozen(tag_.var(), true);
		--problem_.vars;
	}
	btig_.resize((numVars()+1)<<1);
	btig_.markShared(false);
	master()->startInit(constraintGuess);
	lastInit_ = master()->constraints_.size();
	return *master();
}

void SharedContext::addEnumerator(Enumerator* en) {
	enumerator_.reset(en ? en : new NullEnumerator);
	enumerator_->startInit(*this);
}

bool SharedContext::addUnary(Literal p)      { return master()->addUnary(p, Constraint_t::static_constraint); }
void SharedContext::add(Constraint* c)       { return master()->add(c); }
uint32 SharedContext::numConstraints() const { return numBinary() + numTernary() + master()->constraints_.size(); }

bool SharedContext::endInit(bool attachAll) {
	assert(!frozen());
	if (master()->hasConflict()) { return false; }
	master()->setEnumerationConstraint(enumerator()->endInit(*this, concurrency()));
	SatPrepro temp(satPrepro.release());
	bool ok = master()->preparePost() && (!temp.get() || temp->preprocess(*this, enumerator()->enumerate())) && master()->endInit();
	satPrepro.reset(temp.release());
	if (!ok) {
		master()->setEnumerationConstraint(0);
		return false;
	}
	btig_.markShared(concurrency() > 1);
	lastTopLevel_               = master()->assign_.front;
	problem_.constraints        = master()->constraints_.size();
	problem_.constraints_binary = btig_.numBinary();
	problem_.constraints_ternary= btig_.numTernary();
	problem_.size               = std::max(problem_.size, numConstraints());
	share_.frozen = 1;
	for (uint32 i = attachAll ? 1 : solvers_.size(); i != solvers_.size(); ++i) {
		if (!attach(i)) { return false; }
	}
	return true;
}

bool SharedContext::attach(Solver& other) {
	assert(frozen() && other.shared_ == this);
	if (other.validVar(tag_.var())) {
		if (!other.popRootLevel(other.rootLevel())){ return false; }
		if (&other == master())                    { return true;  }
	}
	other.stats.enableStats(master()->stats);
	Var oldV = other.numVars();
	// 1. clone vars & assignment
	other.startInit(static_cast<uint32>(master()->constraints_.size()-lastInit_));
	other.assign_.requestData(master()->assign_.numData());
	LitVec::size_type prevTs = other.trail().size();
	const LitVec& trail      = master()->trail();
	Antecedent null;
	for (LitVec::size_type i = other.assign_.front; i < trail.size(); ++i) {
		if (!other.force(trail[i], null)) {
			return false;
		}
	}
	other.assign_.markUnits();
	other.lastSimplify_ = other.constraints_.empty() ? trail.size() : prevTs;
	if (satPrepro.get()) {
		for (Var v = oldV+1; v <= other.numVars(); ++v) {
			if (eliminated(v) && other.value(v) == value_free) {
				other.assign_.eliminate(v);
			}
		}
	}
	// 2. clone & attach constraints
	const Solver::ConstraintDB& db = master()->constraints_;
	for (LitVec::size_type i = lastInit_; i < db.size(); ++i) {
		if (Constraint* c = db[i]->cloneAttach(other)) {
			other.constraints_.push_back(c);
		}
		if (other.hasConflict()) return false;
	}
	Constraint* c = master()->getEnumerationConstraint();
	other.setEnumerationConstraint( c ? c->cloneAttach(other) : 0 );
	// 3. endInit
	return (other.preparePost() && other.endInit()) || (detach(other, false), false);
}

void SharedContext::detach(Solver& s, bool reset) {
	assert(s.shared_ == this);
	if (reset) { s.reset(); }
	s.removeConditional();
	s.setEnumerationConstraint(0);
}

uint32 SharedContext::problemComplexity() const {
	uint32 r = numBinary() + numTernary();
	for (uint32 i = 0; i != master()->constraints_.size(); ++i) {
		r += master()->constraints_[i]->estimateComplexity(*master());
	}
	return r;
}

SharedLiterals* SharedContext::distribute(const Solver& s, const Literal* lits, uint32 size, const ClauseInfo& extra) const {
	if (distributor.get() && !extra.tagged() && (size <= 3 || opts_.distribute(size, extra.lbd(), extra.type()))) {
		uint32 initialRefs = share_.count - (size <= Clause::MAX_SHORT_LEN || !physicalShare(extra.type()));
		SharedLiterals* x  = SharedLiterals::newShareable(lits, size, extra.type(), initialRefs);
		distributor->publish(s, x);
		const_cast<Solver&>(s).stats.addDistributed(extra.lbd(), extra.type());
		return initialRefs == share_.count ? x : 0;
	}
	return 0;
}

uint32 SharedContext::receive(const Solver& target, SharedLiterals** out, uint32 maxOut) const {
	if (distributor.get()) {
		return distributor->receive(target, out, maxOut);
	}
	return 0;
}
/////////////////////////////////////////////////////////////////////////////////////////
// Distributor
/////////////////////////////////////////////////////////////////////////////////////////
Distributor::Distributor()  {}
Distributor::~Distributor() {}

}
