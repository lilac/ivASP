// 
// Copyright (c) 2006-2012, Benjamin Kaufmann
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
#include <clasp/clasp_facade.h>
#include <clasp/model_enumerators.h>
#include <clasp/cb_enumerator.h>
#include <clasp/weight_constraint.h>
#include <clasp/minimize_constraint.h>
#include <clasp/parallel_solve.h>
#include <stdio.h>
namespace Clasp {
/////////////////////////////////////////////////////////////////////////////////////////
// Default configurations
/////////////////////////////////////////////////////////////////////////////////////////
Configuration::~Configuration() {}
bool Configuration::addPost(Solver& s) const {
	if (s.sharedContext() && s.sharedContext()->sccGraph.get() && !s.hasPost(PostPropagator::priority_reserved_ufs)) {
		return s.addPost(new DefaultUnfoundedCheck());
	}
	return true;
}
void DefaultConfiguration::init(SharedContext& ctx) const {
	ctx.options()          = context_s;
	ctx.master()->strategy = solver(0);
	ctx.satPrepro.reset(0);
}
DecisionHeuristic* DefaultConfiguration::heuristic(uint32) const { return new ClaspBerkmin; }
ContextOptions   DefaultConfiguration::context_s;
SolverStrategies DefaultConfiguration::solver_s;
SolveParams      DefaultConfiguration::search_s;
bool UserConfiguration::addPost(Solver& s) const {
	const SolveParams& p = search(s.id());
	bool  ok             = true;
	if (p.init.lookType != Lookahead::no_lookahead && p.init.lookOps == 0 && !s.hasPost(PostPropagator::priority_reserved_look)) {
		ok = s.addPost(new Lookahead(static_cast<Lookahead::Type>(p.init.lookType)));
	}
	return ok && Configuration::addPost(s);
}
/////////////////////////////////////////////////////////////////////////////////////////
// Heuristics
/////////////////////////////////////////////////////////////////////////////////////////
namespace Heuristic {
DecisionHeuristic* create(const SolverStrategies& str, const SolveParams& p) {
	assert(str.search == SolverStrategies::use_learning || !Heuristic::isLookback(str.heuId));
	typedef DecisionHeuristic DH;
	uint32 heuParam = str.heuParam;
	uint32 id       = str.heuId;
	DH*    heu      = 0;
	if      (id == heu_berkmin) { heu = new ClaspBerkmin(heuParam); }
	else if (id == heu_vmtf)    { heu = new ClaspVmtf(heuParam == 0 ? 8 : heuParam); }
	else if (id == heu_none)    { heu = new SelectFirst(); }
	else if (id == heu_unit)    { heu = new UnitHeuristic(Lookahead::isType(heuParam) ? static_cast<Lookahead::Type>(heuParam) : Lookahead::atom_lookahead); }
	else if (id == heu_vsids)   {
		double m = heuParam == 0 ? 0.95 : heuParam; 
		while (m > 1.0) { m /= 10; } 
		heu = new ClaspVsids(m); 
	}
	else { throw std::runtime_error("Unknown heuristic id!"); }
	if (p.init.lookType != Lookahead::no_lookahead && p.init.lookOps > 0 && id != heu_unit) {
		heu = UnitHeuristic::restricted(static_cast<Lookahead::Type>(p.init.lookType), p.init.lookOps, heu);
	}
	return heu;
}
}
/////////////////////////////////////////////////////////////////////////////////////////
// GlobalOptions
/////////////////////////////////////////////////////////////////////////////////////////
IncrementalControl::IncrementalControl()  {}
IncrementalControl::~IncrementalControl() {}
GlobalOptions::GlobalOptions() : inc(0)   {}
Enumerator* GlobalOptions::createEnumerator(Enumerator::Report* r) {
	ModelEnumerator* e = 0;
	Enumerator* ret    = 0;
	if (consequences()) {
		ret = new CBConsequences(enumerate.mode == enum_brave ? CBConsequences::brave_consequences : CBConsequences::cautious_consequences);
	}
	else if (enumerate.mode == enum_record) {
		ret = (e = new RecordEnumerator());
	}
	else {
		ret = (e = new BacktrackEnumerator(enumerate.projectOpts));
	}
	if (e) { e->setEnableProjection(enumerate.project); }
	ret->setRestartOnModel(enumerate.restartOnModel);
	ret->setReport(r);
	return ret;
}
/////////////////////////////////////////////////////////////////////////////////////////
// ClaspConfig
/////////////////////////////////////////////////////////////////////////////////////////
ClaspConfig::ClaspConfig() : tester_(0) {
	// Master configuration is always neeeded
	ctx().master()->strategy = DefaultConfiguration::solver_s;
	search_.push_back(DefaultConfiguration::search_s);
}

ClaspConfig::~ClaspConfig() {
	ClaspConfig::setConcurrency(1);
	delete tester_;
}

void ClaspConfig::reset() {
	mode.reset();
	setConcurrency(1);
	ctx().master()->strategy = DefaultConfiguration::solver_s;
	search_[0]               = DefaultConfiguration::search_s;
}

const ClaspConfig::SolverOpts& ClaspConfig::solver(uint32 i) const {
	return ctx().solver(i % ctx().concurrency())->strategy;
}
const ClaspConfig::SearchOpts& ClaspConfig::search(uint32 i) const {
	return search_[ i % search_.size() ];
}
DecisionHeuristic* ClaspConfig::heuristic(uint32 i) const {
	return Heuristic::create(ClaspConfig::solver(i), ClaspConfig::search(i));
}
void ClaspConfig::setSatPrepro(SatPreprocessor* p) {
	ctx().satPrepro.reset(p);
}

ClaspConfig::SolverOpts& ClaspConfig::addSolver(uint32 i) {
	while (!ctx().hasSolver(i)) {
		Solver& s  = ctx().addSolver();	
		s.strategy = ctx().solver(0)->strategy;
	}
	return ctx().solver(i)->strategy;
}

ClaspConfig::SearchOpts& ClaspConfig::addSearch(uint32 i) {
	if (i >= search_.size()) {
		search_.resize(i + 1, search_[0]);
	}
	return search_[i];
}

void ClaspConfig::setConcurrency(uint32 num) {
	if (num <= 1) {
		num = 1;
		ctx().options().setDistribution(0,0,0);
	}
	ctx().concurrency(num);
	while (search_.size() > num) { search_.pop_back(); }
}

bool ClaspConfig::estimateComplexity() const {
	for (SolveOpts::const_iterator it = search_.begin(), end = search_.end(); it != end; ++it) {
		if (it->reduce.estimate()) { return true; }
	}
	return false;
}

void ClaspConfig::prepare(ClaspFacade& f) {
	uint32 numS = ctx().concurrency();
	if (incremental() && mode.enumerate.onlyPre) {
		f.warning("'--pre' is ignored in incremental setting."); 
		mode.enumerate.onlyPre = false;
	}
	if (mode.eq.noSCC && mode.eq.iters != 0) {
		f.warning("Selected reasoning mode implies '--eq=0'.");
		mode.eq.noEq();
	}
	if (numS > mode.solve.recommendedSolvers()) {
		char buf[128];
		sprintf(buf, "Oversubscription: #Threads=%u exceeds logical CPUs (%u).", numS, mode.solve.recommendedSolvers());
		f.warning(buf);
		f.warning("Oversubscription leads to excessive context switching.");
	}
	if (mode.opt.all || mode.opt.no) {
		mode.opt.hierarch = 0;
	}
	if (mode.enumerate.numModels == -1 && mode.consequences()) {
		mode.enumerate.numModels = 0;
	}
	if (numS > mode.solve.supportedSolvers()) {
		f.warning("Too many solvers.");
		setConcurrency(mode.solve.supportedSolvers());
	}
	uint32 warn   = 0;
	for (uint32 i = 0, end = ctx().concurrency(), mod = search_.size(); i != end; ++i) {
		warn |= prepareConfig(ctx().solver(i)->strategy, search_[i % mod]);
	}
	if (tester_) {
		warn |= tester_->prepare();
		tester_->setConcurrency(ctx().concurrency());
	}
	if ((warn & 1) != 0) {
		f.warning("Heuristic 'Unit' implies lookahead. Using atom.");
	}
	if ((warn & 2) != 0) {
		f.warning("Heuristic 'Unit' implies unrestricted lookahead.");
	}
	ctx().setConfiguration(this, false);
}

void ClaspConfig::init(SharedContext& ctx) const {
	if (&ctx == &mode.ctx) {
		// Nothing to do because ClaspConfig stores
		// ctx-options and preprocessor directly in 
		// its shared context object
	}
	else { throw std::runtime_error("ClaspConfig::init(): unsupported ctx"); }
}
UserConfiguration* ClaspConfig::addTesterConfig() {
	if (!tester_) { 
		tester_ = new TesterConfig(ctx());
	}
	return tester_;
}
uint32 prepareConfig(SolverStrategies& st, SolveParams& sx) {
	uint32 res = 0;
	if (st.heuId == Heuristic::heu_unit) {
		if      (sx.init.lookType == Lookahead::no_lookahead) { res |= 1; }
		else if (sx.init.lookOps != 0)                        { res |= 2; }
		st.heuParam      = sx.init.lookType == Lookahead::no_lookahead ? Lookahead::atom_lookahead : static_cast<Lookahead::Type>(sx.init.lookType);
		sx.init.lookType = Lookahead::no_lookahead;
		sx.init.lookOps  = 0;
	}
	if (st.search == SolverStrategies::no_learning) {
		sx.restart.disable();
		sx.reduce.disable();
		st.ccMinAntes  = SolverStrategies::no_antes;
		st.strRecursive= 0;
		st.compress    = 0;
		st.saveProgress= 0;
	}
	if (sx.restart.sched.disabled()) { sx.restart.disable(); }
	if (sx.reduce.fReduce() == 0.0f) { sx.reduce.disable();  }
	if (sx.reduce.fMax != 0.0f)      { sx.reduce.fMax = std::max(sx.reduce.fMax, sx.reduce.fInit); }
	return res;
}
/////////////////////////////////////////////////////////////////////////////////////////
// TesterConfig
/////////////////////////////////////////////////////////////////////////////////////////
TesterConfig::TesterConfig(const SharedContext& gen) : concurrency_(gen.concurrency()) {
	solver_.push_back(DefaultConfiguration::solver_s);
	search_.push_back(DefaultConfiguration::search_s);
}
SolverStrategies& TesterConfig::addSolver(uint32 i) {
	if (i >= solver_.size()) {
		solver_.resize(i+1, solver_[0]);
	}
	return solver_[i];
}
SolveParams& TesterConfig::addSearch(uint32 i) {
	if (i >= search_.size()) {
		search_.resize(i+1, search_[0]);
	}
	return search_[i];
}
void TesterConfig::init(SharedContext& ctx) const {
	ctx.options() = ctx_;
	ctx.concurrency(concurrency_);
	for (uint32 i = 1; i != concurrency_; ++i) {
		ctx.addSolver();
	}
	if (satPre_.get()) {
		ctx.satPrepro.reset(satPre_->clone());
	}
	ctx.master()->strategy = solver_[0];
}
uint32 TesterConfig::prepare() {
	uint32 res = 0;
	for (uint32 i = 0; i != solver_.size(); ++i) {
		SolverStrategies& s = solver_[i];
		SolveParams&      sx= search_[i % search_.size()];
		res |= prepareConfig(s, sx);
	}
	return res;
}
DecisionHeuristic* TesterConfig::heuristic(uint32 i) const {
	return Heuristic::create(TesterConfig::solver(i), TesterConfig::search(i));
}
/////////////////////////////////////////////////////////////////////////////////////////
// ClaspFacade
/////////////////////////////////////////////////////////////////////////////////////////
ClaspFacade::ClaspFacade() 
	: config_(0)
	, cb_(0)
	, input_(0)
	, enum_(0)
	, api_(0)
	, result_(result_unknown)
	, state_(num_states)
	, step_(0)
	, more_(true) {
}

void ClaspFacade::init(Input& problem, ClaspConfig& config, Callback* c) {
	config_ = &config;
	cb_     = c;
	input_  = &problem;
	enum_   = config.mode.enumerate.mode != GlobalOptions::enum_auto ? config.mode.createEnumerator(this) : 0;
	api_    = 0;
	result_ = result_unknown;
	state_  = num_states;
	step_   = 0;
	more_   = true;
	if (enum_.get() && !enum_->supportsParallel() && config_->ctx().concurrency() > 1) {
		warning("Selected reasoning mode implies #Threads=1.");
		config_->setConcurrency(1);
	}
	config_->prepare(*this);
}

// Solving...
void ClaspFacade::solve(Input& problem, ClaspConfig& config, Callback* c) {
	init(problem, config, c);
	GlobalOptions& mode     = config.mode;
	const bool onlyPre      = mode.enumerate.onlyPre;
	IncrementalControl* inc = config.incremental();
	AutoState outer(this, state_start);
	LitVec assume;
	do {
		if (inc) { inc->initStep(*this); }
		result_   = result_unknown;
		more_     = true;
		if (config.mode.ctx.master()->hasConflict() || !read(mode) || !preprocess(mode)) {
			result_ = result_unsat;
			more_   = false;
			reportSolution(*config.mode.ctx.enumerator(), true);
			break;
		}
		else if (!onlyPre) {
			assume.clear();
			problem.getAssumptions(assume);
			more_    = solve(assume);
			if (result_ == result_unknown && !more_) {
				// initial assumptions are unsat
				result_ = result_unsat;
			}
		}
	} while (inc && inc->nextStep(*this) && ++step_);
}

// Creates a ProgramBuilder-object if necessary and reads
// the input by calling input_->read().
// Returns false, if the problem is trivially UNSAT.
bool ClaspFacade::read(GlobalOptions& mode) {
	AutoState state(this, state_read);
	Input::ApiPtr ptr(&mode.ctx);
	if (input_->format() == Input::SMODELS) {
		if (step_ == 0) {
			api_   = new ProgramBuilder();
			api_->startProgram(mode.ctx, mode.eq);
			api_->setNonHcfConfiguration(config_->testerConfig());
		}
		if (mode.inc) {
			api_->updateProgram();
		}
		ptr.api= api_.get();
	}
	if (mode.opt.hierarch > 0 && !mode.opt.no) {
		mode.ctx.requestTagLiteral();
	}
	uint32 properties = !mode.enumerate.maxSat || input_->format() != Input::DIMACS ? 0 : Input::AS_MAX_SAT;
	if (mode.enumerate.numModels != 1) { 
		properties |= mode.enumerate.numModels == -1 ? Input::PRESERVE_MODELS_ON_MIN : Input::PRESERVE_MODELS;
	}
	return input_->read(ptr, properties);
}

// Prepare the solving state:
//  - if necessary, transforms the input to nogoods by calling ProgramBuilder::endProgram()
//  - fires event_p_prepared after input was transformed to nogoods
//  - adds any minimize statements to the solver and initializes the enumerator
//  - calls Solver::endInit().
// Returns false, if the problem is trivially UNSAT.
bool ClaspFacade::preprocess(GlobalOptions& mode) {
	AutoState state(this, state_preprocess);
	SharedContext&    ctx = mode.ctx;
	SharedMinimizeData* m = 0;
	Input::ApiPtr ptr(&ctx);
	if (api_.get()) {
		if (!api_->endProgram()) {
			fireEvent(*ctx.master(), event_p_prepared);
			return false;
		}
		ptr.api = api_.get();
	}
	if (!mode.opt.no && step_ == 0) {
		MinimizeBuilder builder;
		input_->addMinimize(builder, ptr);
		if (builder.hasRules()) {
			if (!mode.opt.vals.empty()) {
				const SumVec& opt = mode.opt.vals;
				for (uint32 i = 0; i != opt.size(); ++i) {
					builder.setOptimum(i, opt[i]);
				}
			}
			m = builder.build(ctx, ctx.tagLiteral());
			if (!m) { return false; }
		}
		if (!builder.hasRules() || (builder.numRules() == 1 && mode.opt.hierarch < 2)) {
			ctx.removeTagLiteral();
		}
	}
	fireEvent(*ctx.master(), event_p_prepared);
	if (!mode.inc && api_.is_owner()) {
		api_ = 0;
	}
	return mode.enumerate.onlyPre || (initEnumeration(m) && initContextObject(ctx));
}

// Finalizes initialization of model enumeration.
// Configures and adds an eventual minimize constraint,
// sts the number of models to compute and adds warnings
// if this number conflicts with the preferred number of the enumerator.
bool ClaspFacade::initEnumeration(SharedMinimizeData* min)  {
	GlobalOptions::EnumOptions& mode = config_->mode.enumerate;
	MinimizeMode minMode             = !min || config_->mode.opt.all ? MinimizeMode_t::enumerate : MinimizeMode_t::optimize;
	if (step_ == 0) {
		GlobalOptions::EnumMode autoMode = GlobalOptions::enum_bt;
		if (mode.restartOnModel) {
			autoMode = GlobalOptions::enum_record;
		}
		if (minMode == MinimizeMode_t::optimize && !mode.project) {
			autoMode = GlobalOptions::enum_record;
		}
		if (mode.project && concurrency() > 1) {
			autoMode = GlobalOptions::enum_record;
		}
		uint32 autoModels = !min && !mode.consequences();
		if (autoModels == 0 && mode.numModels > 0) {
			if (mode.consequences())                { warning("'--number' not 0: last model may not cover consequences.");   }
			if (minMode == MinimizeMode_t::optimize){ warning("'--number' not 0: optimality of last model not guaranteed."); }
		}
		if (mode.numModels == -1) { mode.numModels = autoModels; }	
		if (mode.consequences() && minMode == MinimizeMode_t::optimize) {
			warning("Optimization: Consequences may depend on enumeration order.");
		}
		if (mode.project && minMode == MinimizeMode_t::optimize) {
			for (const WeightLiteral* it = min->lits; !isSentinel(it->first); ++it) {
				if ( !config_->mode.ctx.project(it->first.var()) ) {
					warning("Projection: Optimization values may depend on enumeration order.");
					break;
				}
			}
		}
		if (mode.mode == GlobalOptions::enum_auto) {
			mode.mode = autoMode;
			enum_ = config_->mode.createEnumerator(this);
		}
	}
	if (min) {
		min->setMode(minMode, config_->mode.opt.hierarch);
		enum_->setMinimize(min);
	}
	enum_->enumerate(mode.numModels);
	config_->mode.ctx.addEnumerator(enum_.release());
	return true;
}

// Finalizes initialization of SharedContext and
// computes a value that represents the problem size.
// The value is then used by the reduce-heuristic
// to determine the initial learnt db size.
bool ClaspFacade::initContextObject(SharedContext& ctx) const {
	if (!ctx.endInit()) { return false; }
	uint32 estimate = config_->estimateComplexity() ? ctx.problemComplexity() : 0;
	uint32 size     = ctx.numConstraints();
	if (input_->format() != Input::DIMACS) {
		double r = ctx.numVars() / std::max(1.0, double(ctx.numConstraints()));
		if (r < 0.1 || r > 10.0) {
			size = std::max(ctx.numVars(), ctx.numConstraints());
		}
		else {
			size = std::min(ctx.numVars(), ctx.numConstraints());
		}
	}
	ctx.setProblemSize(size, estimate);
	return true;
}

bool ClaspFacade::solve(const LitVec& assume) {
	struct OnExit : AutoState { 
		OnExit(ClaspFacade* f) : AutoState(f, state_solve) {}
		~OnExit() { SolveAlgorithm* x = self_->config_->ctx().solve; self_->config_->ctx().solve = 0; delete x; }
	};
	OnExit resetSolve(this);
	config_->mode.solve.createSolveObject(config_->ctx());
	SolveAlgorithm* algo = config_->ctx().solve;
	bool more = algo->solve(config_->ctx(), config_->search(0), assume);
	if (algo->hasErrors() && concurrency() > 1) {
		uint64 failMask = algo->hasErrors();
		for (uint32 i = config_->mode.ctx.concurrency(); i && failMask; ) {
			uint32 idMask = uint64(1) << (--i);
			if ((failMask & idMask) != 0) {
				char buf[128];
				sprintf(buf, "Thread %u failed and was removed.", i);
				warning(buf);
				failMask &= ~idMask;
			}
		} 
	}
	return more;
}
}
