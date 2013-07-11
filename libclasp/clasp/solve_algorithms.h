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
#ifndef CLASP_SOLVE_ALGORITHMS_H_INCLUDED
#define CLASP_SOLVE_ALGORITHMS_H_INCLUDED

#ifdef _MSC_VER
#pragma warning (disable : 4200) // nonstandard extension used : zero-sized array
#pragma once
#endif

#include <clasp/solver_strategies.h>

/*!
 * \file 
 * Defines top-level functions for solving problems.
 */
namespace Clasp { 

//! Type for holding global solve limits.
struct SolveLimits {
	explicit SolveLimits(uint64 conf = UINT64_MAX, uint64 r = UINT64_MAX) 
		: conflicts(conf)
		, restarts(r) {
	}
	bool   reached() const { return conflicts == 0 || restarts == 0; }
	uint64 conflicts; /*!< Number of conflicts. */
	uint64 restarts;  /*!< Number of restarts.  */
};
///////////////////////////////////////////////////////////////////////////////
// Basic solve functions
///////////////////////////////////////////////////////////////////////////////

//! Basic sequential search.
/*!
 * \ingroup solver
 * \relates Solver
 * \param ctx The context containing the problem.
 * \param p   The solve parameters to use.
 * \param lim Optional solve limit. 
 *
 * \return
 *  - true:  if the search stopped before the search-space was exceeded.
 *  - false: if the search-space was completely examined.
 * 
 */
bool solve(SharedContext& ctx, const SolveParams& p, const SolveLimits& lim = SolveLimits());

//! Basic sequential search under assumptions.
/*!
 * \ingroup solver
 * \relates Solver
 * The use of assumptions allows for incremental solving. Literals contained
 * in assumptions are assumed to be true during search but are undone before solve returns.
 *
 * \param ctx The context containing the problem.
 * \param p   The solve parameters to use.
 * \param assumptions The list of initial unit-assumptions.
 * \param lim Optional solve limit.
 *
 * \return
 *  - true:  if the search stopped before the search-space was exceeded.
 *  - false: if the search-space was completely examined.
 * 
 */
bool solve(SharedContext& ctx, const SolveParams& p, const LitVec& assumptions, const SolveLimits& lim = SolveLimits());

///////////////////////////////////////////////////////////////////////////////
// General solve
///////////////////////////////////////////////////////////////////////////////
struct SolveEvent_t { enum Type {progress_msg = 1, progress_state = 2, progress_path = 3, progress_test = 4}; };
struct SolveMsgEvent : SolveEvent {
	SolveMsgEvent(const Solver& s, const char* m) : SolveEvent(s, SolveEvent_t::progress_msg), msg(m) {}
	const char* msg;
};
struct SolveStateEvent : SolveEvent {
	SolveStateEvent(const Solver& s, const char* m, double exitTime = -1.0) : SolveEvent(s, SolveEvent_t::progress_state), state(m), time(exitTime) {}
	const char* state; // state that is entered or left
	double      time;  // < 0: enter, else exit
};
struct SolvePathEvent : SolveEvent {
	enum EventType { event_none = 0, event_deletion = 'D', event_grow = 'G', event_model = 'M', event_restart = 'R' };
	SolvePathEvent(const Solver& s, EventType t, uint64 cLim, uint32 lLim) : SolveEvent(s, SolveEvent_t::progress_path), cLimit(cLim), lLimit(lLim), evType(t) {}
	uint64    cLimit; // next conflict limit
	uint32    lLimit; // next learnt limit
	EventType evType; // type of path event
};
struct SolveTestEvent : SolveEvent {
	SolveTestEvent(const Solver& s, uint32 scc, bool partial);
	int    result;     // -1: before test, 0: unstable, 1: stable
	uint32 scc     :31;// scc under test
	uint32 partial : 1;// partial test?
	uint64 confDelta;  // conflicts before test
	uint64 choiceDelta;// choices before test
	double timeDelta;  // time before test
	
	uint64 conflicts() const;
	uint64 choices()   const;
	double time()      const;
};

//! Interface for solve algorithms.
/*!
 * \ingroup solver
 * \relates Solver
 * SolveAlgorithm objects wrap an enumerator and
 * implement concrete solve algorithms.
 */
class SolveAlgorithm {
public:
	/*!
	 * \param lim    An optional solve limit applied in solve().
	 */
	explicit SolveAlgorithm(const SolveLimits& limit = SolveLimits());
	virtual ~SolveAlgorithm();

	//! Force termination of current solve process.
	/*!
	 * Shall return true if termination is supported, otherwise false.
	 */
	virtual bool   terminate()       { return false; }
	virtual bool   terminated()const { return false; }
	virtual uint64 hasErrors() const { return 0;     }

	//! Runs the solve algorithm.
	/*!
	 * \param ctx    A fully initialized context object containing the problem.
	 * \param s      The solver used for searching (typically ctx.master())
	 * \param p      The solve parameters for the given solver.
	 * \param assume A list of initial unit-assumptions.
	 *
	 * \return
	 *  - true:  if the search stopped before the search-space was exceeded.
	 *  - false: if the search-space was completely examined.
	 *
	 * \note 
	 * The use of assumptions allows for incremental solving. Literals contained
	 * in assumptions are assumed to be true during search but are undone before solve returns.
	 * \note 
	 * The function is implemented in terms of beginSolve(ctx, s), doSolve(s, p, assume), endSolve(ctx, s).
	 */
	bool           solve(SharedContext& ctx, Solver& s, const SolveParams& p, const LitVec& assume);
	bool           solve(SharedContext& ctx, const SolveParams& p, const LitVec& assume);
	//! Checks whether the given problem is satisfiable under the given assumptions.
	virtual bool   satisfiable(SharedContext& ctx, Solver& s, const SolveParams& p, const LitVec& assume);
	//! The default implementation simply forwards the call to the enumerator.
	virtual bool   backtrackFromModel(Solver& s);
protected:
	//! Pre-Solve hook.
	/*!
	 * The default implementation returns ctx.attach(s);
	 */
	virtual bool  beginSolve(SharedContext& ctx, Solver& s);
	//! Post-Solve hook.
	/*!
	 * The default implementation detaches s from ctx and reports the result 
	 * via the enumerator's report interface.
	 */
	virtual bool  endSolve(SharedContext& ctx, Solver& s, bool solveRet);
	//! The actual solve algorithm.
	/*!
	 * The default implmentation uses single-threaded (single solver) 
	 * sequential solving.
	 */
	virtual bool  doSolve(Solver& s, const SolveParams& p, const LitVec& assume);
	
	const SolveLimits& getSolveLimits()const { return limits_; }
	void               setSolveLimits(const SolveLimits& x) { limits_ = x; }
private:
	SolveAlgorithm(const SolveAlgorithm&);
	SolveAlgorithm& operator=(const SolveAlgorithm&);
	SolveLimits limits_;
};

//! Solves the path stored in the given solver using the given solving options.
/*!
 * \param st   An active solve algorithm or 0. If st is not null, st->backtrackFromModel(s)
 *             is called whenever a model is found.
 * \param s    A solver with valid (possibly empty) path attached to a problem (SharedContext).
 * \param p    Solving options to apply during search.
 * \param init If true, p.init.ramdomize(s) is called before the actual search is started.
 * \param lim  Solving limits to apply (in/out parameter). 
 * \return
 *    - value_true  if search stopped on a model.
 *    - value_false if the search-space was completely examined.
 *    - value_free  if a solve limit was hit.
 */
ValueRep solvePath(SolveAlgorithm* st, Solver& s, const SolveParams& p, bool init, SolveLimits& lim);

}
#endif
