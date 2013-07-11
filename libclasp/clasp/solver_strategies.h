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
#ifndef CLASP_SOLVER_STRATEGIES_H_INCLUDED
#define CLASP_SOLVER_STRATEGIES_H_INCLUDED
#ifdef _MSC_VER
#pragma once
#endif

#include <clasp/constraint.h>
#include <clasp/util/misc_types.h>

/*!
 * \file 
 * Contains strategies and options used to configure solvers and search.
 */
namespace Clasp {

//! Implements clasp's configurable schedule-strategies.
/*!
 * clasp currently supports the following basic strategies:
 *  - geometric sequence  : X = n1 * n2^k   (k >= 0)
 *  - arithmetic sequence : X = n1 + (n2*k) (k >= 0)
 *  - fixed sequence      : X = n1 + (0*k)  (k >= 0)
 *  - luby's sequence     : X = n1 * luby(k)(k >= 0) 
 *  .
 * Furthermore, an inner-outer scheme can be applied to the selected sequence.
 * In that case, the sequence is repeated every <limit>+j restarts, where
 * <limit> is the initial outer-limit and j is the number of times the
 * sequence was already repeated.
 *
 * \note For luby's seqeuence, j is not a repetition counter
 * but the index where the sequence grows to the next power of two.
 *
 * \see Luby et al. "Optimal speedup of las vegas algorithms."
 *
 */
struct ScheduleStrategy {
public:
	enum Type { geometric_schedule = 0, arithmetic_schedule = 1, luby_schedule = 2, user_schedule = 3 }; 
	
	ScheduleStrategy(Type t = geometric_schedule, uint32 b = 100, double g = 1.5, uint32 o = 0);
	//! Creates luby's sequence with unit-length unit and optional outer limit.
	static ScheduleStrategy luby(uint32 unit, uint32 limit = 0)              { return ScheduleStrategy(luby_schedule, unit, 0, limit);  }
	//! Creates geometric sequence base * (grow^k) with optional outer limit.
	static ScheduleStrategy geom(uint32 base, double grow, uint32 limit = 0) { return ScheduleStrategy(geometric_schedule, base, grow, limit);  }
	//! Creates arithmetic sequence base + (add*k) with optional outer limit.
	static ScheduleStrategy arith(uint32 base, double add, uint32 limit = 0) { return ScheduleStrategy(arithmetic_schedule, base, add, limit);  }
	//! Creates fixed sequence with length base.
	static ScheduleStrategy fixed(uint32 base)                               { return ScheduleStrategy(arithmetic_schedule, base, 0, 0);  }
	static ScheduleStrategy none()                                           { return ScheduleStrategy(geometric_schedule, 0); }
	static ScheduleStrategy def()                                            { return ScheduleStrategy(user_schedule, 0, 0.0); }
	uint64 current()  const;
	bool   disabled() const { return base == 0; }
	bool   defaulted()const { return base == 0 && type == user_schedule; }
	void   reset()          { idx  = 0;         }
	uint64 next();
	uint32 base : 30;  // base of sequence (n1)
	uint32 type :  2;  // type of basic sequence
	uint32 idx;        // current index into sequence
	uint32 len;        // length of sequence (0 if infinite) (once reached, sequence is repeated and len increased)
	float  grow;       // update parameter n2
	
};

uint32 lubyR(uint32 idx);
double growR(uint32 idx, double g);
double addR(uint32 idx, double a);
inline uint32 log2(uint32 x) {
	uint32 ln = 0;
	if (x & 0xFFFF0000u) { x >>= 16; ln |= 16; }
	if (x & 0xFF00u    ) { x >>=  8; ln |=  8; }
	if (x & 0xF0u      ) { x >>=  4; ln |=  4; }
	if (x & 0xCu       ) { x >>=  2; ln |=  2; }
	if (x & 0x2u       ) {/*x>>=1*/; ln |=  1; }
	return ln;
}

//! Reduce strategy used during solving.
/*!
 * A reduce strategy mainly consists of an algorithm and a scoring scheme
 * for measuring "activity" of learnt constraints.
 */
struct ReduceStrategy {
	//! Reduction algorithm to use during solving.
	enum Algorithm {
		reduce_linear   = 0, /*!< Linear algorithm from clasp-1.3.x. */
		reduce_stable   = 1, /*!< Sort constraints by score but keep order in learnt db. */
		reduce_sort     = 2, /*!< Sort learnt db by score and remove fraction with lowest score. */
		reduce_heap     = 3  /*!< Similar to reduce_sort but only partially sorts learnt db.  */
	};
	//! Score to measure "activity" of learnt constraints.
	enum Score {
		score_act  = 0, /*!< Activity only: how often constraint is used during conflict analysis. */
		score_lbd  = 1, /*!< Use literal block distance as activity. */
		score_both = 2  /*!< Use activity and lbd together. */
	};
	static uint32 scoreAct(const Activity& act)  { return act.activity(); }
	static uint32 scoreLbd(const Activity& act)  { return uint32(128)-act.lbd(); }
	static uint32 scoreBoth(const Activity& act) { return (act.activity()+1) * scoreLbd(act); }
	ReduceStrategy() : glue(0), fReduce(75), fRestart(0), score(0), algo(0), noGlue(0), estimate(0) {}
	static int    compare(Score sc, const Clasp::Activity& lhs, const Clasp::Activity& rhs) {
		int fs = 0;
		if      (sc == score_act) { fs = ((int)scoreAct(lhs)) - ((int)scoreAct(rhs)); }
		else if (sc == score_lbd) { fs = ((int)scoreLbd(lhs)) - ((int)scoreLbd(rhs)); }
		return fs != 0 ? fs : ((int)scoreBoth(lhs)) - ((int)scoreBoth(rhs)); 
	}
	static uint32 asScore(Score sc, const Clasp::Activity& act) {
		if (sc == score_act)  { return scoreAct(act); }
		if (sc == score_lbd)  { return scoreLbd(act); }
		/*  sc == score_both*/{ return scoreBoth(act);}
	}
	uint32 glue    : 8; /*!< Don't remove nogoods with lbd <= glue.    */
	uint32 fReduce : 7; /*!< Fraction of nogoods to remove in percent. */
	uint32 fRestart: 7; /*!< Fraction of nogoods to remove on restart. */
	uint32 score   : 2; /*!< One of Score.                             */
	uint32 algo    : 2; /*!< One of Algorithm.                         */
	uint32 noGlue  : 1; /*!< Do not count glue clauses in limit        */
	uint32 estimate: 1; /*!< Use estimate of problem complexity in init*/
};

class DecisionHeuristic;

//! Parameter-Object for configuring a solver.
struct SolverStrategies {
	//! Clasp's two general search strategies
	enum SearchStrategy {
		use_learning = 0, /*!< Analyze conflicts and learn First-1-UIP-clause */
		no_learning  = 1  /*!< Don't analyze conflicts - chronological backtracking */
	};
	//! Antecedents to consider during conflict clause minimization.
	enum CCMinAntes {
		no_antes     = 0,  /*!< Don't minimize first-uip-clauses. */
		all_antes    = 1,  /*!< Consider all antecedents.         */
		short_antes  = 2,  /*!< Consider only short antecedents.  */
		binary_antes = 3,  /*!< Consider only binary antecedents. */
	};
	enum OptHeu {
		opt_sign     = 1,  /*!< Use optimize statements in sign heuristic */ 
		opt_model    = 2,  /*!< Apply model heuristic when optimizing */
	};
	enum SignDef {
		sign_type    = 0, /*!< prefer literal based on var's type */
		sign_no      = 1, /*!< prefer positive literal */
		sign_yes     = 2, /*!< prefer negative literal */
		sign_rnd     = 3, /*!< prefer random literal   */
	};
	enum WatchInit  { watch_first = 0, watch_rand = 1, watch_least = 2 };
	enum UpdateMode { update_on_propagate = 0, update_on_conflict  = 1 };
	//! Creates a default-initialized object.
	SolverStrategies();
	//----- 64 bit ------------	
	uint32    heuParam     : 16; /*!< Extra parameter for heuristic with meaning depending on type */
	uint32    memLimit     : 16; /*!< If > 0, enable soft memory limit on learnt nogoods. */
	uint32    compress     : 16; /*!< If > 0, enable compression for learnt clauses of size > compress. */
	uint32    saveProgress : 16; /*!< Enable progress saving if > 0. */
	//-------------------------
	//----- 32 bit ------------
	uint32    heuId        : 3;  /*!< Type of decision heuristic. */
	uint32    ccMinAntes   : 2;  /*!< Antecedents to look at during conflict clause minimization. */
	uint32    loopRep      : 2;  /*!< How to represent loops? */
	uint32    reverseArcs  : 2;  /*!< Use "reverse-arcs" during learning if > 0. */
	uint32    otfs         : 2;  /*!< Enable "on-the-fly" subsumption if > 0. */
	uint32    updateLbd    : 2;  /*!< Update lbds of antecedents during conflict analysis. */
	uint32    initWatches  : 2;  /*!< Initialize watches randomly in clauses. */
	uint32    heuOther     : 2;  /*!< Consider other learnt nogoods in heuristic (0=no, 1=loops, 2=all, 3=let heuristic decide) */
	uint32    optHeu       : 2;  /*!< Set of OptHeu values. */
	uint32    signDef      : 2;  /*!< One of SignDef. */
	uint32    signFix      : 1;  /*!< Disable all sign heuristics and always use default sign. */
	uint32    disjTrue     : 1;  /*!< Treat atoms in disjunctions as bodies in type-based sign heuristic. */
	uint32    upMode       : 1;  /*!< One of UpdateMode. */
	uint32    heuReinit    : 1;  /*!< Enable/disable reinitialization of existing vars in incremental setting */
	uint32    heuMoms      : 1;  /*!< Use MOMS-score as top-level heuristic */
	uint32    berkHuang    : 1;  /*!< Only for Berkmin. */
	uint32    berkOnce     : 1;  /*!< Only for Berkmin. */
	uint32    unitNant     : 1;  /*!< Only for unit.    */
	uint32    bumpVarAct   : 1;  /*!< Bump activities of vars implied by learnt clauses with small lbd. */
	uint32    strRecursive : 1;  /*!< If 1, use more expensive recursive nogood minimization. */
	uint32    search       : 1;  /*!< Current search strategy. */
	//-------------------------
	//----- 32 bit ------------
	RNG       rng;               /*!< RNG used during search.    */
	//-------------------------
};

typedef Range<uint32> Range32;
	
//! Aggregates restart-parameters to configure restarts during search.
/*!
 * \see ScheduleStrategy
 */
struct RestartParams {
	RestartParams() : sched(), counterRestart(0), counterBump(9973), shuffle(0), shuffleNext(0), cntLocal(0), dynRestart(0), bndRestart(0), rstRestart(0) {}
	void disable();
	bool dynamic()     const { return dynRestart != 0; }
	bool local()       const { return cntLocal   != 0; }
	bool bounded()     const { return bndRestart != 0; }
	bool resetOnModel()const { return rstRestart != 0; }
	ScheduleStrategy sched; /**< Restart schedule to use. */
	uint16 counterRestart;  /**< Apply counter implication bump every counterRestart restarts (0: disable). */
	uint16 counterBump;     /**< Bump factor for counter implication restarts. */
	uint32 shuffle    :14;  /**< Shuffle program after shuffle restarts (0: disable). */
	uint32 shuffleNext:14;  /**< Re-Shuffle program every shuffleNext restarts (0: disable). */
	uint32 cntLocal   : 1;  /**< Count conflicts globally or relative to current branch? */
	uint32 dynRestart : 1;  /**< Dynamic restarts enabled? */
	uint32 bndRestart : 1;  /**< Allow (bounded) restarts after first solution was found. */
	uint32 rstRestart : 1;  /**< Repeat restart seq. after each solution. */
};

//! Aggregates parameters for the nogood deletion heuristic used during search.
struct ReduceParams {
	ReduceParams() : cflSched(ScheduleStrategy::none()), growSched(ScheduleStrategy::def())
		, fInit(1.0f/3.0f)
		, fMax(3.0f)
		, fGrow(1.1f)
		, initRange(10, UINT32_MAX)
		, maxRange(UINT32_MAX) {}
	void   disable();
	uint32 getBase(const SharedContext& ctx)   const;
	uint32 initLimit(const SharedContext& ctx) const { return getLimit(getBase(ctx), fInit, initRange); }
	uint32 maxLimit(const SharedContext& ctx)  const { 
		return getLimit(getBase(ctx), fMax, Range32(initLimit(ctx), maxRange)); 
	}
	bool   estimate() const { return strategy.estimate != 0; }
	float  fReduce()  const { return strategy.fReduce / 100.0f; }
	float  fRestart() const { return strategy.fRestart/ 100.0f; }
	static uint32 getLimit(uint32 base, double f, const Range<uint32>& r);
	ScheduleStrategy cflSched;   /**< Conflict-based deletion schedule.               */
	ScheduleStrategy growSched;  /**< Growth-based deletion schedule.                 */
	ReduceStrategy   strategy;   /**< Strategy to apply during nogood deletion.       */
	float            fInit;      /**< Initial limit. X = P*fInit clamped to initRange.*/
	float            fMax;       /**< Maximal limit. X = P*fMax  clamped to maxRange. */
	float            fGrow;      /**< Growth factor for db.                           */
	Range32          initRange;  /**< Allowed range for initial limit.                */
	uint32           maxRange;   /**< Allowed range for maximal limit: [initRange.lo,maxRange]*/
};

//! Type for holding pre-solve options.
struct InitParams {
	InitParams() : randRuns(0), randConf(0), lookType(0), lookOps(0) {}
	//! Sets the randomization-parameters to use during path initialization.
	/*!
	 * \param runs Number of initial randomized-runs.
	 * \param cfl  Number of conflicts comprising one randomized-run.
	 */
	bool setRandomizeParams(uint32 runs, uint32 cfls) {
		if (!runs || !cfls) { runs = cfls = 0; }
		randRuns = (uint16)std::min(runs, (1u<<16)-1);
		randConf = (uint16)std::min(cfls, (1u<<16)-1);
		return true;
	}
	bool randomize(Solver& s) const;
	uint16 randRuns; /*!< Number of initial randomized-runs. */
	uint16 randConf; /*!< Number of conflicts comprising one randomized-run. */
	uint16 lookType; /*!< Type of lookahead operations. */
	uint16 lookOps;  /*!< Max. number of lookahead operations (0: no limit). */
};

//! Parameter-Object for grouping solve-related options.
/*!
 * \ingroup solver
 */
struct SolveParams {
	//! Creates a default-initialized object.
	/*!
	 * The following parameters are used:
	 * restart      : quadratic: 100*1.5^k / no restarts after first solution
	 * deletion     : initial size: vars()/3, grow factor: 1.1, max factor: 3.0, do not reduce on restart
	 * randomization: disabled
	 * randomProp   : 0.0 (disabled)
	 */
	SolveParams();	
	RestartParams restart;
	ReduceParams  reduce;
	InitParams    init;
	float         randProb;  /*!< Use random heuristic with given probability ([0,1]) */
	struct FwdCheck {        /*!< Options for partial checks in DLP-solving; */
		uint32 initHigh : 24;/*!< Init high level to this DL (0 = max level) */
		uint32 highPct  :  7;/*!< Check on low + (high - low) * highPct/100  */
		uint32 incHigh  :  1;/*!< Inc high level when reached. */
		FwdCheck() { *reinterpret_cast<uint32*>(this) = 0; }
	}             fwdCheck;
};

//! Parameter-Object for configuring a shared context.
struct ContextOptions {
	ContextOptions();
	//! How to handle short learnt clauses.
	enum ShortMode  { 
		short_implicit = 0, /*!< Share short learnt clauses via short implication graph. */
		short_explicit = 1, /*!< Do not use short implication graph. */
	};
	//! How to handle physical sharing of (explicit) constraints.
	enum ShareMode  { 
		share_no      = 0, /*!< Do not physically share constraints (use copies instead). */
		share_problem = 1, /*!< Share problem constraints but copy learnt constraints.    */
		share_learnt  = 2, /*!< Copy problem constraints but share learnt constraints.    */
		share_all     = 3  /*!< Share all constraints.                                    */
	};
	//! Sets limits on distribution.
	void   setDistribution(uint32 maxSize, uint32 maxLbd, uint32 types) {
		distSize = maxSize;
		distLbd  = maxLbd;
		distMask = types;
	}
	bool   distribute(uint32 size, uint32 lbd, uint32 type) const { return size <= distSize && lbd <= distLbd && ((type & distMask) != 0); }
	uint32 distSize  : 18; /*!< Allow distribution up to this size only. */
	uint32 distLbd   :  8; /*!< Allow distribution up to this lbd only.  */
	uint32 distMask  :  3; /*!< Restrict distribution to this type.      */
	uint32 shareMode :  2; /*!< One of ShareMode. */
	uint32 shortMode :  1; /*!< One of ShortMode. */
};

class SatPreprocessor;
class SharedContext;

//! Interface for configuring a SharedContext object and its associated solvers.
class Configuration {
public:
	typedef ContextOptions    CtxOpts;
	typedef SolverStrategies  SolverOpts;
	typedef SolveParams       SearchOpts;
	virtual ~Configuration();
	//! Initializes the given SharedContext from this configuration.
	virtual void               init(SharedContext&) const = 0;
	//! Returns the solver options for the i'th solver to be attached to the SharedContext.
	virtual const SolverOpts&  solver(uint32 i)     const = 0;
	//! Returns the search options for the i'th solver of the SharedContext.
	virtual const SearchOpts&  search(uint32 i)     const = 0;
	//! Returns the heuristic to be used in the i'th solver.
	/*!
	 * The function is called in Solver::startInit().
	 * \note The returned object is owned by the caller.
	 */
	virtual DecisionHeuristic* heuristic(uint32 i)  const = 0;
	//! Adds post propagators to the given solver.
	/*!
	 * The function is called during initialization of s.
	 * The default implementation adds a post propagator for unfounded set checking
	 * if necessary. 
	 */
	virtual bool               addPost(Solver& s)   const;
};

//! A configuration that uses default values for all objects.
class DefaultConfiguration : public Configuration {
public:
	void              init(SharedContext&) const;
	const SolverOpts& solver(uint32)       const { return solver_s; }
	const SearchOpts& search(uint32)       const { return search_s; }
	DecisionHeuristic*heuristic(uint32)    const;
	static CtxOpts    context_s;
	static SolverOpts solver_s;
	static SearchOpts search_s;
};

//! Base class for user-provided configurations.
class UserConfiguration : public Configuration {
public:
	//! Adds post propagators to the given solver.
	/*!
	 * The function is called during initialization of s.
	 * The default implementation calls Configuration::addPost(s)
	 * and adds a lookahead post propagator if necessary.
	 */
	virtual bool            addPost(Solver& s)   const;
	//! Sets the number of solvers that can share the SharedContext.
	virtual void            setConcurrency(uint32 num)       = 0;
	//! Sets the preprocessor to be used in the SharedContext.
	virtual void            setSatPrepro(SatPreprocessor* p) = 0;
	//! Returns the options to be applied to the SharedContext.
	virtual ContextOptions* context()                        = 0;
	//! Returns the (modifiable) solver options for the i'th solver.
	virtual SolverOpts&     addSolver(uint32 i)              = 0;
	//! Returns the (modifiable) search options for the i'th solver.
	virtual SearchOpts&     addSearch(uint32 i)              = 0;
};

}
#endif
