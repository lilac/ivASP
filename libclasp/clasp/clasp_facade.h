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
#ifndef CLASP_CLASP_FACADE_H_INCLUDED
#define CLASP_CLASP_FACADE_H_INCLUDED

#ifdef _MSC_VER
#pragma warning (disable : 4200) // nonstandard extension used : zero-sized array
#pragma once
#endif

#if !defined(CLASP_VERSION)
#define CLASP_VERSION "2.2-TP (Rev. $Revision$)"
#endif
#if !defined(CLASP_LEGAL)
#define CLASP_LEGAL \
"Copyright (C) Benjamin Kaufmann\n"\
"License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>\n"\
"clasp is free software: you are free to change and redistribute it.\n"\
"There is NO WARRANTY, to the extent permitted by law."
#endif

#if !defined(WITH_THREADS)
#error Invalid thread configuration - use WITH_THREADS=0 for single-threaded or WITH_THREADS=1 for multi-threaded version of libclasp!
#endif

#if WITH_THREADS
#include <clasp/parallel_solve.h>
typedef Clasp::mt::ParallelSolveOptions SolveOptions;
#else
#include <clasp/solve_algorithms.h>
#include <clasp/shared_context.h>
namespace Clasp {
struct SolveOptions {
	SolveLimits   limit;  /**< Solve limit (disabled by default). */
	void   createSolveObject(SharedContext& ctx) const { ctx.solve = new SolveAlgorithm(limit); }
	static uint32 supportedSolvers()   { return 1; }
	static uint32 recommendedSolvers() { return 1; }
};
}
#endif
#include <clasp/literal.h>
#include <clasp/solver.h>
#include <clasp/enumerator.h>
#include <clasp/heuristics.h>
#include <clasp/lookahead.h>
#include <clasp/program_builder.h>
#include <clasp/unfounded_check.h>
#include <clasp/reader.h>
#include <clasp/util/misc_types.h>
#include <string>

/*!
 * \file 
 * This file provides a facade around the clasp library. 
 * I.e. a simplified interface for (incrementally) solving a problem using
 * some configuration (set of parameters).
 */
namespace Clasp {

class ClaspFacade;

//! Interface for controling incremental solving.
class IncrementalControl {
public:
	IncrementalControl();
	virtual ~IncrementalControl(); 
	//! Called before an incremental step is started.
	virtual void initStep(ClaspFacade& f)  = 0;
	//! Called after an incremental step finished.
	/*!
	 * \return
	 *  - true to signal that solving should continue with next step
	 *  - false to terminate the incremental solving loop
	 */
	virtual bool nextStep(ClaspFacade& f)  = 0;
private:
	IncrementalControl(const IncrementalControl&);
	IncrementalControl& operator=(const IncrementalControl&);
};

/////////////////////////////////////////////////////////////////////////////////////////
// Configuration
/////////////////////////////////////////////////////////////////////////////////////////	
namespace Heuristic {
	enum Type { heu_berkmin = 0, heu_vsids = 1, heu_vmtf = 2, heu_unit = 3, heu_none = 4  };
	inline bool        isLookback(uint32 type) { return type != heu_none && type != heu_unit; }
	DecisionHeuristic* create(const SolverStrategies&, const SolveParams&);
}
//! Configuration object for configuring co-np tests of disjunctive programs.
class TesterConfig : public UserConfiguration {
public:
	explicit TesterConfig(const SharedContext& generator);
	// Base interface
	DecisionHeuristic* heuristic(uint32)    const;
	void               init(SharedContext&) const;
	const SolverOpts&  solver(uint32 i)     const       { return solver_[i%solver_.size()]; }
	const SearchOpts&  search(uint32 i)     const       { return search_[i%search_.size()]; }
	Configuration*     tester()             const       { return 0; }
	void               setConcurrency(uint32 num)       { concurrency_ = num; }
	void               setSatPrepro(SatPreprocessor* p) { satPre_.reset(p); }
	ContextOptions*    context()                        { return &ctx_; }
	SolverOpts&        addSolver(uint32 i);
	SearchOpts&        addSearch(uint32 i);
	//
	uint32             prepare();
private:
	typedef PodVector<SolverOpts>::type SolverVec;
	typedef PodVector<SearchOpts>::type SearchVec;
	SolverVec                      solver_;
	SearchVec                      search_;
	std::auto_ptr<SatPreprocessor> satPre_;
	ContextOptions                 ctx_;
	uint32                         concurrency_;
};

//! Global options controlling overall solving algorithm.
struct GlobalOptions {
public:
	typedef ProgramBuilder::EqOptions EqOptions;
	typedef std::auto_ptr<Enumerator> EnumPtr;
	enum EnumMode      { enum_auto = 0, enum_bt = 1, enum_record = 2, enum_consequences = 4, enum_brave = 5, enum_cautious = 6 };
	GlobalOptions();
	Enumerator*     createEnumerator(Enumerator::Report* r = 0);
	bool consequences() const { return enumerate.consequences(); }
	const char* cbType()const { return consequences() ? (enumerate.mode == enum_brave  ? "Brave" : "Cautious") : "none"; }
	SharedContext ctx;    /**< Context-object used by all solvers. */
	SolveOptions  solve;  /**< Additional options for solving. */
	EqOptions     eq;     /**< Options for equivalence preprocessing. */
	struct Optimize {     /**< Optimization options. */
		Optimize() : hierarch(0), no(false), all(false) {}
		SumVec vals;        /**< Initial values for optimize statements. */
		uint32 hierarch;    /**< Use hierarchical optimization scheme. */
		bool   no;          /**< Ignore optimize statements. */
		bool   all;         /**< Compute all models <= vals. */
	}             opt;
	struct EnumOptions {  /**< Enumeration options. */
		EnumOptions() : numModels(-1), mode(enum_auto), projectOpts(7), project(false)
		              , maxSat(false), restartOnModel(false), onlyPre(false)  {}
		bool consequences() const { return (mode & enum_consequences) != 0; }
		int       numModels;     /**< Number of models to compute. */
		EnumMode  mode;          /**< Enumeration type to use. */
		uint32    projectOpts;   /**< Options for projection. */
		bool      project;       /**< Enable projection. */
		bool      maxSat;        /**< Treat DIMACS input as MaxSat */
		bool      restartOnModel;/**< Restart after each model. */
		bool      onlyPre;       /**< Stop after preprocessing step? */
	}             enumerate;
	IncrementalControl* inc;   /**< Incremental solving? */
	void reset() {
		ctx.reset();
		solve     = SolveOptions();
		eq        = EqOptions();
		opt       = Optimize();
		enumerate = EnumOptions();
		inc       = 0;
	}
};

//! Configuration object for configuring all supported aspects of solving.
class ClaspConfig : public UserConfiguration {
public:
	typedef UserConfiguration  UserConfig;
	typedef IncrementalControl IncrementalCtrl;
	typedef Solver**           SolverIt;
	ClaspConfig();
	~ClaspConfig();
	// Base interface
	DecisionHeuristic*heuristic(uint32)    const;
	void              init(SharedContext&) const;
	const SolverOpts& solver(uint32)       const;
	const SearchOpts& search(uint32)       const;
	void              setConcurrency(uint32 num);
	void              setSatPrepro(SatPreprocessor* p);
	ContextOptions*   context()            { return &ctx().options(); }
	SolverOpts&       addSolver(uint32 i);
	SearchOpts&       addSearch(uint32 i);
	// 
	SharedContext&    ctx()              { return mode.ctx; }
	IncrementalCtrl*  incremental() const{ return mode.inc; }
	bool              onlyPre()     const{ return mode.enumerate.onlyPre; }
	UserConfig*       testerConfig()const{ return tester_; } 
	UserConfig*       addTesterConfig();
	void              prepare(ClaspFacade& f);
	bool              estimateComplexity() const;
	void              reset();
	GlobalOptions     mode;
private:
	ClaspConfig(const ClaspConfig&);
	ClaspConfig& operator=(const ClaspConfig&);
	typedef PodVector<SolveParams>::type  SolveOpts;
	const SharedContext& ctx() const { return mode.ctx; }
	SolveOpts     search_;
	TesterConfig* tester_;
};
uint32 prepareConfig(SolverStrategies&, SolveParams&);
/////////////////////////////////////////////////////////////////////////////////////////
// ClaspFacade
/////////////////////////////////////////////////////////////////////////////////////////
//! Provides a simplified interface for (incrementally) solving a given problem.
class ClaspFacade : public Enumerator::Report {
public:
	//! Defines the possible solving states.
	enum State { 
		state_start,       /*!< Computation started. */
		state_read,        /*!< Problem is read from input. */
		state_preprocess,  /*!< Problem is prepared. */
		state_solve,       /*!< Search is active. */
		num_states
	};
	//! Defines important event types.
	enum Event { 
		event_state_enter, /*!< A new state was entered. */
		event_state_exit,  /*!< About to exit from the active state. */
		event_p_prepared,  /*!< Problem was transformed to nogoods. */
		event_model        /*!< A model was found. */
	};
	//! Defines possible solving results.
	enum Result { result_unsat, result_sat, result_unknown }; 
	//! Callback interface for notifying about important steps in solve process.
	class Callback {
	public:
		virtual ~Callback() {}
		//! State transition. Called on entering/exiting a state.
		/*!
		 * \param e Either event_state_enter or event_state_exit.
		 * \note Call f.state() to get the active state.
		 */
		virtual void state(Event e, ClaspFacade& f) = 0;
		//! Some operation triggered an important event.
		/*!
		 * \param s The solver that triggered the event.
		 * \param e An event that is neither event_state_enter nor event_state_exit.
		 */
		virtual void event(const Solver& s, Event e, ClaspFacade& f) = 0;
		//! Some configuration option is unsafe/unreasonable w.r.t the current problem.
		virtual void warning(const char* msg)       = 0;
	};
	ClaspFacade();
	
	/*!
	 * Solves the problem given in problem using the given configuration.
	 * \note Once solve() returned, the result of the computation can be
	 * queried via the function result().
	 * \note If config.onlyPre() is true, solve() returns after
	 * the preprocessing step (i.e. once the solver is prepared) and does not start a search.
	 *
	 * \note If config.incremental() is != 0, solve() runs in a loop
	 *       until config.incremental().nextStep(*this) returns false.
	 *
	 */
	void solve(Input& problem, ClaspConfig& config, Callback* c);

	//! Returns the result of a computation.
	Result result() const { return result_; }
	//! Returns true if search-space was completed. Otherwise false.
	bool   more()   const { return more_; }
	//! Returns the active state.
	State  state()  const { return state_; }
	//! Returns the current incremental step (starts at 0).
	int    step()   const { return step_; }
	//! Returns the current input problem.
	Input* input() const { return input_; }
	//! Tries to terminate an active search.
	bool   terminate() const { return config_ && config_->ctx().solve && config_->ctx().solve->terminate(); }
	
	const ClaspConfig* config() const { return config_; }

	//! Returns the ProgramBuilder-object that was used to transform a logic program into nogoods.
	/*!
	 * \note A ProgramBuilder-object is only created if input()->format() == Input::SMODELS
	 * \note The ProgramBuilder-object is destroyed after the event
	 *       event_p_prepared was fired. Call releaseApi to disable auto-deletion of api.
	 *       In that case you must later manually delete it!
	 */
	ProgramBuilder* api() const  { return api_.get();     }
	ProgramBuilder* releaseApi() { return api_.release(); }

	void warning(const char* w) const { if (cb_) cb_->warning(w); }
private:
	ClaspFacade(const ClaspFacade&);
	ClaspFacade& operator=(const ClaspFacade&);
	struct AutoState {
		AutoState(ClaspFacade* f, State s) : self_(f), state_(s) { self_->setState(s, event_state_enter); }
		~AutoState() { self_->setState(state_, event_state_exit); }
		ClaspFacade* self_;
		State        state_;
	};
	typedef SingleOwnerPtr<ProgramBuilder> Api;
	typedef SingleOwnerPtr<Enumerator>     EnumPtr;
	// -------------------------------------------------------------------------------------------  
	// Status information
	void setState(State s, Event e)          { state_ = s; if (cb_) cb_->state(e, *this); }
	void fireEvent(const Solver& s, Event e) { if (cb_) cb_->event(s, e, *this); }
	// -------------------------------------------------------------------------------------------
	// Enumerator::Report interface
	void reportModel(const Solver& s, const Enumerator&) {
		result_ = result_sat;
		fireEvent(s, event_model);
	}
	void reportSolution(const Enumerator& e, bool complete) {
		more_ = !complete;
		if (!more_ && e.enumerated == 0) {
			result_ = result_unsat;
		}
	}
	// -------------------------------------------------------------------------------------------
	// Internal setup functions
	void   init(Input&, ClaspConfig&, Callback* c);
	bool   read(GlobalOptions& mode);
	bool   preprocess(GlobalOptions& mode);
	bool   solve(const LitVec& assume);
	bool   initEnumeration(SharedMinimizeData* min);
	bool   initContextObject(SharedContext& ctx) const;
	uint32 concurrency() const { return config_->ctx().concurrency(); }
	// -------------------------------------------------------------------------------------------
	ClaspConfig*    config_;
	Callback*       cb_;
	Input*          input_;
	EnumPtr         enum_;
	Api             api_;
	Result          result_;
	State           state_;
	int             step_;
	bool            more_;
};

}
#endif
