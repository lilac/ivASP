// 
// Copyright (c) 2006-2011, Benjamin Kaufmann
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
#ifndef CLASP_SOLVER_TYPES_H_INCLUDED
#define CLASP_SOLVER_TYPES_H_INCLUDED
#ifdef _MSC_VER
#pragma once
#endif

#include <clasp/literal.h>
#include <clasp/constraint.h>
#include <clasp/util/left_right_sequence.h>
#include <clasp/util/misc_types.h>
#include <clasp/util/type_manip.h>
#include <numeric>
/*!
 * \file 
 * Contains some types used by a Solver
 */
namespace Clasp {
class SharedLiterals;

/*!
 * \addtogroup solver
 */
//@{

///////////////////////////////////////////////////////////////////////////////
// Statistics
///////////////////////////////////////////////////////////////////////////////
inline double ratio(uint64 x, uint64 y) {
	if (!x || !y) return 0.0;
	return static_cast<double>(x) / static_cast<double>(y);
}
inline double percent(uint64 x, uint64 y) {	return ratio(x, y) * 100.0; }

//! A struct for holding core statistics used by a solver.
/*!
 * Core statistics are always present in a solver and hence
 * can be used by heuristics.
 */
struct CoreStats {
	CoreStats() { reset(); }
	void reset() { std::memset(this, 0, sizeof(CoreStats)); }
	void accu(const CoreStats& o) {
		uint64*        x = reinterpret_cast<uint64*>(this);
		const uint64* ox = reinterpret_cast<const uint64*>(&o);
		const uint64* oe = &o.cflLast;
		while (ox != oe) { *x++ += *ox++; }
		cflLast          = std::max(cflLast, o.cflLast);
	}
	uint64 backtracks() const { return conflicts-analyzed; }
	uint64 backjumps()  const { return analyzed; }
	double avgRestart() const { return ratio(analyzed, restarts); }
	uint64 choices;   /**< Number of choices performed. */
	uint64 conflicts; /**< Number of conflicts found. */
	uint64 analyzed;  /**< Number of number of analyzed conflicts. */
	uint64 restarts;  /**< Number of restarts. */ 
	uint64 cflLast;   /**< Number of conflicts since last restart. */
};

//! A struct for holding (optional) extra statistics.
struct ExtendedStats {
	typedef ConstraintType type_t;
	ExtendedStats() { reset(); }
	void reset() { std::memset(this, 0, sizeof(ExtendedStats)); }
	void accu(const ExtendedStats& o) {
		uint64*        x = reinterpret_cast<uint64*>(this);
		const uint64* ox = reinterpret_cast<const uint64*>(&o);
		const uint64* oe = reinterpret_cast<const uint64*>(o.lits + Constraint_t::max_value);
		while (ox != oe) { *x++ += *ox++; }
		binary   += o.binary;
		ternary  += o.ternary;
		cpuTime  += o.cpuTime;
		intImps  += o.intImps;
		intJumps += o.intJumps;
		gpLits   += o.gpLits;
		gps      += o.gps;
		splits   += o.splits;
	}
	void addLearnt(uint32 size, type_t t) {
		assert(t != Constraint_t::static_constraint && t <= Constraint_t::max_value);
		learnts[t-1]+= 1;
		lits[t-1]   += size;
		binary      += (size == 2);
		ternary     += (size == 3);
	}
	uint64 sumLemmas()     const { return std::accumulate(learnts, learnts+Constraint_t::max_value, uint64(0)); }
	uint64 num(type_t t)   const { return learnts[t-1]; }
	double avgLen(type_t t)const { return ratio(lits[t-1], num(t)); }
	double avgModel()      const { return ratio(modLits, models);    }
	double distRatio()     const { return ratio(distributed, learnts[0] + learnts[1]);  }
	double avgDistLbd()    const { return ratio(sumDistLbd, distributed); }
	double avgIntJump()    const { return ratio(intJumps, intImps); }
	double avgGp()         const { return ratio(gpLits, gps); }
	double intRatio()      const { return ratio(integrated, distributed); }
	// CL1
	uint64 models;     /**< Number of models found in one solver.   */
	uint64 modLits;    /**< Sum of decision literals in models.     */
	uint64 numTests;   /**< Number of stability tests (only in DLP).*/
	uint64 numPartial; /**< Number of partial stability tests.      */
	uint64 deleted;    /**< Sum of learnt nogoods removed.          */
	uint64 distributed;/**< Number of nogoods distributed.          */
	uint64 sumDistLbd; /**< Sum of lbds of distributed nogoods.     */
	uint64 integrated; /**< Number of nogoods integrated            */
	// CL2
	typedef uint64 Array[Constraint_t::max_value];
	Array  learnts;    /**< learnts[t-1]: Number of learnt nogoods of type t.  */
	Array  lits;       /**< lits[t-1]   : Sum of literals in nogoods of type t.*/
	uint32 binary;     /**< Number of learnt binary nogoods.                   */
	uint32 ternary;    /**< Number of learnt ternary nogoods.                  */
	double cpuTime;    /**< (Estimated) cpu time of the current solver.        */
	// CL3
	uint64 intImps;    /**< Number of initial implications from integrated nogoods.*/
	uint64 intJumps;   /**< Sum of backjumps needed to integrate new implications. */
	uint64 gpLits;     /**< Sum of literals in received guiding paths.             */
	uint32 gps;        /**< Number of guiding paths received.                      */
	uint32 splits;     /**< Number of split requests handled.                      */
};

//! A struct for holding (optional) jump statistics.
struct JumpStats {
	JumpStats() { reset(); }
	void reset(){ std::memset(this, 0, sizeof(*this)); }
	void accu(const JumpStats& o) {
		jumps   += o.jumps;
		bJumps  += o.bJumps;
		jumpSum += o.jumpSum;
		boundSum+= o.boundSum;
		maxJump  = std::max(o.maxJump, maxJump);
		maxJumpEx= std::max(o.maxJumpEx, maxJumpEx);
		maxBound = std::max(o.maxBound, maxBound);
	}
	void update(uint32 dl, uint32 uipLevel, uint32 bLevel) {
		++jumps;
		jumpSum += dl - uipLevel; 
		maxJump = std::max(maxJump, dl - uipLevel);
		if (uipLevel < bLevel) {
			++bJumps;
			boundSum += bLevel - uipLevel;
			maxJumpEx = std::max(maxJumpEx, dl - bLevel);
			maxBound  = std::max(maxBound, bLevel - uipLevel);
		}
		else { maxJumpEx = maxJump; }
	}
	uint64 jumped()     const { return jumpSum - boundSum; }
	double jumpedRatio()const { return ratio(jumped(), jumpSum); }
	double avgBound()   const { return ratio(boundSum, bJumps); }
	double avgJump()    const { return ratio(jumpSum, jumps); }
	double avgJumpEx()  const { return ratio(jumped(), jumps); }
	uint64 jumps;    /**< Number of jumps performed.                                            */
	uint64 bJumps;   /**< Number of backjumps that were bounded.                                */
	uint64 jumpSum;  /**< Number of levels that could be skipped w.r.t first-uip.               */
	uint64 boundSum; /**< Number of levels that could not be skipped because of backtrack-level.*/
	uint32 maxJump;  /**< Longest possible backjump.                                            */
	uint32 maxJumpEx;/**< Longest executed backjump (< maxJump if longest jump was bounded).    */
	uint32 maxBound; /**< Max difference between uip- and backtrack-level.                      */
};

struct QueueImpl {
	explicit QueueImpl(uint32 size) : maxSize(size), wp(0), rp(0) {}
	bool    full()  const { return size() == maxSize; }
	uint32  size()  const { return ((rp > wp) * cap()) + wp - rp;}
	uint32  cap()   const { return maxSize + 1; }
	void    clear()       { wp = rp = 0; }
	uint32  top()   const { return buffer[rp]; }
	void    push(uint32 x){ buffer[wp] = x; if (++wp == cap()) { wp = 0; } }
	void    pop()         { if (++rp == cap()) { rp = 0; } }
	uint32  maxSize;
	uint32  wp;
	uint32  rp;
	uint32  buffer[1];
};

struct SumQueue {
	static SumQueue* create(uint32 size) {
		void* m = ::operator new(sizeof(SumQueue) + (size*sizeof(uint32)));
		return new (m) SumQueue(size);
	}
	void dynamicRestarts(float x, bool xLbd) {
		upForce  = 16000;
		upCfl    = 0;
		nRestart = 0;
		lim      = x;
		lbd      = xLbd;
	}
	void destroy()         { this->~SumQueue(); ::operator delete(this); }
	void resetQueue()      { sumLbd = sumCfl = samples = 0; queue.clear(); }
	void resetGlobal()     { globalSumLbd = globalSumCfl = globalSamples = 0; resetQueue(); }
	void update(uint32 dl, uint32 lbd) {
		if (samples++ >= queue.maxSize) {
			uint32 y = queue.top(); 
			sumLbd  -= (y & 127u);
			sumCfl  -= (y >> 7u);
			queue.pop();
		}
		sumLbd += lbd;
		sumCfl += dl;
		++upCfl;
		++globalSamples;
		globalSumLbd += lbd;
		globalSumCfl += dl;
		queue.push((dl << 7) + lbd);
	}
	double  avgLbd() const { return sumLbd / (double)queue.maxSize; }
	double  avgCfl() const { return sumCfl / (double)queue.maxSize; }
	uint32  maxSize()const { return queue.maxSize; }
	bool    full()   const { return queue.full();  }
	double  globalAvgLbd() const { return ratio(globalSumLbd, globalSamples); }
	double  globalAvgCfl() const { return ratio(globalSumCfl, globalSamples); } 
	bool    isRestart()    const { return full() && (lbd ? (avgLbd()*lim) > globalAvgLbd() : (avgCfl() * lim) > globalAvgCfl()); }
	uint32  restart(uint32 maxLBD, float xLim);

	uint64    globalSumLbd; /**< Sum of lbds since last call to resetGlobal().           */
	uint64    globalSumCfl; /**< Sum of conflict levels since last call to resetGlobal().*/
	uint64    globalSamples;/**< Samples since last call to resetGlobal().               */
	uint32    sumLbd;       /**< Sum of lbds in queue.            */
	uint32    sumCfl;       /**< Sum of conflict levels in queue. */
	uint32    samples;      /**< Number of items in queue.        */
	// ------- Dynamic restarts -------
	uint32    upForce;      /**< Number of conflicts before an update is forced.*/
	uint32    upCfl;        /**< Conflicts since last update.                   */
	uint32    nRestart;     /**< Restarts since last update.                    */
	float     lim;          /**< LBD/CFL adjustment factor for dynamic restarts (0=disable). */
	bool      lbd;          /**< Dynamic restarts based on true=lbd or false=confllict level.*/
	// --------------------------------
	QueueImpl queue;
private: 
	SumQueue(uint32 size) 
		: globalSumLbd(0), globalSumCfl(0), globalSamples(0), sumLbd(0), sumCfl(0), samples(0)
		, upForce(16000), upCfl(0), nRestart(0), lim(0.0f), lbd(true)
		, queue(size) {
	}
	SumQueue(const SumQueue&);
	SumQueue& operator=(const SumQueue&);
};

//! A struct for aggregating statistics maintained in a solver object.
struct SolveStats : public CoreStats {
	SolveStats() : queue(0), extra(0), jumps(0) {}
	SolveStats(const SolveStats& o) : CoreStats(o), queue(0), extra(0), jumps(0) {
		if (o.queue) enableQueue(o.queue->maxSize());
		enableStats(o);
	}
	~SolveStats() { delete jumps; delete extra; if (queue) queue->destroy(); }
	void enableStats(const SolveStats& other);
	void enableExtended();
	void enableJump();
	void enableQueue(uint32 size);
	void reset();
	void accu(const SolveStats& o);
	void swapStats(SolveStats& o);
	inline void addLearnt(uint32 size, ConstraintType t);
	inline void updateJumps(uint32 dl, uint32 uipLevel, uint32 bLevel, uint32 lbd);
	inline void addDeleted(uint32 num);
	inline void addDistributed(uint32 lbd, ConstraintType t);
	inline void addTest(bool partial);
	inline void addModel(uint32 decisionLevel);
	inline void addCpuTime(double t);
	inline void addSplit(uint32 num = 1);
	inline void addIntegratedAsserting(uint32 receivedDL, uint32 jumpDL);
	inline void addIntegrated(uint32 num = 1);
	inline void removeIntegrated(uint32 num = 1);
	inline void addPath(const LitVec::size_type& sz);
	SumQueue*      queue; /**< Optional queue for running averages. */
	ExtendedStats* extra; /**< Optional extended statistics.        */
	JumpStats*     jumps; /**< Optional jump statistics.            */
private: SolveStats& operator=(const SolveStats&);
};
inline void SolveStats::addLearnt(uint32 size, ConstraintType t)  { if (extra) { extra->addLearnt(size, t); } }
inline void SolveStats::addDeleted(uint32 num)                    { if (extra) { extra->deleted += num; }  }
inline void SolveStats::addDistributed(uint32 lbd, ConstraintType){ if (extra) { ++extra->distributed; extra->sumDistLbd += lbd; } }
inline void SolveStats::addIntegrated(uint32 n)                   { if (extra) { extra->integrated += n;} }
inline void SolveStats::removeIntegrated(uint32 n)                { if (extra) { extra->integrated -= n;} }
inline void SolveStats::addCpuTime(double t)                      { if (extra) { extra->cpuTime += t; }    }
inline void SolveStats::addSplit(uint32 num)                      { if (extra) { extra->splits += num; }  }
inline void SolveStats::addPath(const LitVec::size_type& sz)      { if (extra) { ++extra->gps; extra->gpLits += sz; } }
inline void SolveStats::addTest(bool partial)                     { if (extra) { ++extra->numTests; extra->numPartial += (uint32)partial; } }
inline void SolveStats::addModel(uint32 DL)                       { if (extra) { ++extra->models; extra->modLits += DL; } }
inline void SolveStats::addIntegratedAsserting(uint32 rDL, uint32 jDL) {
	if (extra) { ++extra->intImps; extra->intJumps += (rDL - jDL); }
}
inline void SolveStats::updateJumps(uint32 dl, uint32 uipLevel, uint32 bLevel, uint32 lbd) {
	++analyzed;
	if (queue) { queue->update(dl, lbd); }
	if (jumps) { jumps->update(dl, uipLevel, bLevel); }
}
///////////////////////////////////////////////////////////////////////////////
// Clauses
///////////////////////////////////////////////////////////////////////////////
//! Type storing initial information on a (learnt) clause.
class ClauseInfo {
public:
	typedef ClauseInfo self_type;
	enum { MAX_LBD = Activity::MAX_LBD, MAX_ACTIVITY = (1<<22)-1 }; 
	ClauseInfo(ConstraintType t = Constraint_t::static_constraint) : act_(0), lbd_(MAX_LBD), type_(t), tag_(0) {
		static_assert(sizeof(self_type) == sizeof(uint32), "Unsupported padding");
	}
	bool           learnt()   const { return type() != Constraint_t::static_constraint; }
	ConstraintType type()     const { return static_cast<ConstraintType>(type_); }
	uint32         activity() const { return static_cast<uint32>(act_); }
	uint32         lbd()      const { return static_cast<uint32>(lbd_); }
	bool           tagged()   const { return tag_ != 0; }
	self_type&     setType(ConstraintType t) { type_  = static_cast<uint32>(t); return *this; }
	self_type&     setActivity(uint32 act)   { act_   = std::min(act, (uint32)MAX_ACTIVITY); return *this; }
	self_type&     setTagged(bool b)         { tag_   = static_cast<uint32>(b); return *this; }
	self_type&     setLbd(uint32 a_lbd)      { lbd_   = std::min(a_lbd, (uint32)MAX_LBD); return *this; }
private:
	uint32 act_ : 22; // Activity of clause
	uint32 lbd_ :  7; // Literal block distance in the range [0, MAX_LBD]
	uint32 type_:  2; // One of ConstraintType
	uint32 tag_ :  1; // Conditional constraint?
};
//! (Abstract) base class for clause types.
/*!
 * ClauseHead is used to enforce a common memory-layout for all clauses.
 * It contains the two watched literals and a cache literal to improve
 * propagation performance. A virtual call to Constraint::propagate()
 * is only needed if the other watch is not true and the cache literal
 * is false.
 */
class ClauseHead : public LearntConstraint {
public:
	enum { HEAD_LITS = 3, MAX_SHORT_LEN = 5, MAX_LBD = (1<<5)-1, TAGGED_CLAUSE = 1023, MAX_ACTIVITY = (1<<15)-1 };
	explicit ClauseHead(const ClauseInfo& init);
	// base interface
	//! Propagates the head and calls updateWatch() if necessary.
	PropResult propagate(Solver& s, Literal, uint32& data);
	//! Type of clause.
	ConstraintType type() const    { return ConstraintType(info_.data.type); }
	//! True if this clause currently is the antecedent of an assignment.
	bool     locked(const Solver& s) const;
	//! Returns the activity of this clause.
	Activity activity() const       { return Activity(info_.data.act, info_.data.lbd); }
	//! Halves the activity of this clause.
	void     decreaseActivity()     { info_.data.act >>= 1; }
	//! Downcast from LearntConstraint.
	ClauseHead* clause()           { return this; }
	
	// clause interface
	typedef std::pair<bool, bool> BoolPair;
	//! Increases activity.
	void bumpActivity()     { info_.data.act += (info_.data.act != MAX_ACTIVITY); }
	//! Adds watches for first two literals in head to solver.
	void attach(Solver& s);
	//! Returns true if head is satisfied w.r.t current assignment in s.
	bool satisfied(const Solver& s);
	//! Conditional clause?
	bool tagged() const     { return info_.data.key == uint32(TAGGED_CLAUSE); }
	bool learnt() const     { return info_.data.type!= 0; }
	uint32 lbd()  const     { return info_.data.lbd; }
	void lbd(uint32 x)      { info_.data.lbd = std::min(x, uint32(MAX_LBD)); }
	//! Removes watches from s.
	virtual void     detach(Solver& s);
	//! Returns the size of this clause.
	virtual uint32   size()              const = 0;
	//! Returns the literals of this clause in out.
	virtual void     toLits(LitVec& out) const = 0;
	//! Returns true if this clause is a valid "reverse antecedent" for p.
	virtual bool     isReverseReason(const Solver& s, Literal p, uint32 maxL, uint32 maxN) = 0;
	//! Removes p from clause if possible.
	/*!
	 * \return
	 *   The first component of the returned pair specifies whether or not
	 *   p was removed from the clause.
	 *   The second component of the returned pair specifies whether
	 *   the clause should be kept (false) or removed (true). 
	 */
	virtual BoolPair strengthen(Solver& s, Literal p, bool allowToShort = true) = 0;
protected:
	friend struct ClauseWatch;
	bool         toImplication(Solver& s);
	void         clearTagged()   { info_.data.key = 0; }
	void         setLbd(uint32 x){ info_.data.lbd = x; }
	bool         hasLbd() const  { return info_.data.type != Constraint_t::learnt_other || lbd() != MAX_LBD; }
	//! Shall replace the watched literal at position pos with a non-false literal.
	/*!
	 * \pre pos in [0,1] 
	 * \pre s.isFalse(head_[pos]) && s.isFalse(head_[2])
	 * \pre head_[pos^1] is the other watched literal
	 */
	virtual bool updateWatch(Solver& s, uint32 pos) = 0;
	union Data {
		SharedLiterals* shared;
		struct LocalClause {
			uint32 sizeExt;
			uint32 idx;
			void   init(uint32 size) {
				if (size <= ClauseHead::MAX_SHORT_LEN){ sizeExt = idx = negLit(0).asUint(); }
				else                                  { sizeExt = (size << 3) + 1; idx = 0; }
			}
			bool   isSmall()     const    { return (sizeExt & 1u) == 0u; }
			bool   contracted()  const    { return (sizeExt & 3u) == 3u; }
			bool   strengthened()const    { return (sizeExt & 5u) == 5u; }
			uint32 size()        const    { return sizeExt >> 3; }
			void   setSize(uint32 size)   { sizeExt = (size << 3) | (sizeExt & 7u); }
			void   markContracted()       { sizeExt |= 2u;  }
			void   markStrengthened()     { sizeExt |= 4u;  }
			void   clearContracted()      { sizeExt &= ~2u; }
		}               local;
		uint32          lits[2];
	}       data_;   // additional data
	union Info { 
		Info() : rep(0) {}
		explicit Info(const ClauseInfo& i);
		struct {
			uint32 act : 15; // activity of clause
			uint32 key : 10; // lru key of clause
			uint32 lbd :  5; // lbd of clause
			uint32 type:  2; // type of clause
		}      data;
		uint32 rep;
	}       info_;
	Literal head_[HEAD_LITS]; // two watched literals and one cache literal
};
//! Allocator for small (at most 32-byte) clauses.
class SmallClauseAlloc {
public:
	SmallClauseAlloc();
	~SmallClauseAlloc();
	void* allocate() {
		if(freeList_ == 0) {
			allocBlock();
		}
		Chunk* r   = freeList_;
		freeList_  = r->next;
		return r;
	}
	void   free(void* mem) {
		Chunk* b = reinterpret_cast<Chunk*>(mem);
		b->next  = freeList_;
		freeList_= b;
	}
private:
	SmallClauseAlloc(const SmallClauseAlloc&);
	SmallClauseAlloc& operator=(const SmallClauseAlloc&);
	struct Chunk {
		Chunk*        next; // enforce ptr alignment
		unsigned char mem[32 - sizeof(Chunk*)];
	};
	struct Block {
		enum { num_chunks = 1023 };
		Block* next;
		unsigned char pad[32-sizeof(Block*)];
		Chunk  chunk[num_chunks];
	};
	void allocBlock();
	Block*  blocks_;
	Chunk*  freeList_;
};
///////////////////////////////////////////////////////////////////////////////
// Watches
///////////////////////////////////////////////////////////////////////////////
//! Represents a clause watch in a Solver.
struct ClauseWatch {
	//! Clause watch: clause head
	explicit ClauseWatch(ClauseHead* a_head) : head(a_head) { }
	ClauseHead* head;
	struct EqHead {
		explicit EqHead(ClauseHead* h) : head(h) {}
		bool operator()(const ClauseWatch& w) const { return head == w.head; }
		ClauseHead* head;
	};
};

//! Represents a generic watch in a Solver.
struct GenericWatch {
	//! A constraint and some associated data.
	explicit GenericWatch(Constraint* a_con, uint32 a_data = 0) : con(a_con), data(a_data) {}
	//! Calls propagate on the stored constraint and passes the stored data to that constraint.
	Constraint::PropResult propagate(Solver& s, Literal p) { return con->propagate(s, p, data); }
	
	Constraint* con;    /**< The constraint watching a certain literal. */
	uint32      data;   /**< Additional data associated with this watch - passed to constraint on update. */

	struct EqConstraint {
		explicit EqConstraint(Constraint* c) : con(c) {}
		bool operator()(const GenericWatch& w) const { return con == w.con; }
		Constraint* con;
	};	
};

//! Watch list type.
typedef bk_lib::left_right_sequence<ClauseWatch, GenericWatch, 0> WatchList;
inline void releaseVec(WatchList& w) { w.clear(true); }

///////////////////////////////////////////////////////////////////////////////
// Assignment
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
//! Type for storing reasons for variable assignments together with additional data.
/*!
 * \note On 32-bit systems additional data is stored in the high-word of antecedents.
 */
struct ReasonStore32 : PodVector<Antecedent>::type {
	uint32  dataSize() const     { return (uint32)size(); }
	void    dataResize(uint32)   {}
	uint32  data(uint32 v) const { return decode((*this)[v]);}
	void    setData(uint32 v, uint32 data) { encode((*this)[v], data); }
	static  void   encode(Antecedent& a, uint32 data) {
		a.asUint() = (uint64(data)<<32) | static_cast<uint32>(a.asUint());
	}
	static  uint32 decode(const Antecedent& a) {
		return static_cast<uint32>(a.asUint()>>32);
	}
	struct value_type {
		value_type(const Antecedent& a, uint32 d) : ante_(a) {
			if (d != UINT32_MAX) { encode(ante_, d); assert(data() == d && ante_.type() == Antecedent::generic_constraint); }
		}
		const Antecedent& ante() const { return ante_;      }
		      uint32      data() const { return ante_.type() == Antecedent::generic_constraint ? decode(ante_) : UINT32_MAX; }
		Antecedent ante_;
	};
};

//! Type for storing reasons for variable assignments together with additional data.
/*
 * \note On 64-bit systems additional data is stored in a separate container.
 */
struct ReasonStore64 : PodVector<Antecedent>::type {
	uint32  dataSize() const               { return (uint32)data_.size(); }
	void    dataResize(uint32 nv)          { if (nv > dataSize()) data_.resize(nv, UINT32_MAX); }
	uint32  data(uint32 v) const           { return data_[v]; }
	void    setData(uint32 v, uint32 data) { dataResize(v+1); data_[v] = data; }
	VarVec  data_;
	struct  value_type : std::pair<Antecedent, uint32> {
		value_type(const Antecedent& a, uint32 d) : std::pair<Antecedent, uint32>(a, d) {}
		const Antecedent& ante() const { return first;  }
		      uint32      data() const { return second; }
	};
};

//! A set of configurable values for a variable.
/*!
 * Beside its currently assigned value, a variable
 * can also have a user, saved, preferred, and default value.
 * These values are used in sign selection to determine the signed literal 
 * of a variable to be assign first.
 * During sign selection, the values form a hierarchy:
 * user > saved > preferred > current sign score of heuristic > default value
 */
struct ValueSet {
	ValueSet() : rep(0) {}
	enum Value { user_value = 0x03u, saved_value = 0x0Cu, pref_value = 0x30u, def_value = 0xC0u };
	bool     sign()       const { return (right_most_bit(rep) & 0xAAu) != 0; }
	bool     empty()      const { return rep == 0; }
	bool     has(Value v) const { return (rep & v) != 0; }
	bool     has(uint32 f)const { return (rep & f) != 0; }
	ValueRep get(Value v) const { return static_cast<ValueRep>((rep & v) / right_most_bit(v)); }
	void     set(Value which, ValueRep to) { rep &= ~which; rep |= (to * right_most_bit(which)); }
	void     save(ValueRep x)   { rep &= ~saved_value; rep |= (x << 2); }
	uint8 rep;
};

//! Stores assignment related information.
/*!
 * For each variable v, the class stores 
 *  - v's current value (value_free if unassigned)
 *  - the decision level on which v was assign (only valid if value(v) != value_free)
 *  - the reason why v is in the assignment (only valid if value(v) != value_free)
 *  - (optionally) some additional data associated with the reason
 *  .
 * Furthermore, the class stores the sequences of assignments as a set of
 * true literals in its trail-member.
 */
class Assignment  {
public:
	typedef PodVector<uint32>::type     AssignVec;
	typedef PodVector<ValueSet>::type   PrefVec;
	typedef bk_lib::detail::if_then_else<
		sizeof(Constraint*)==sizeof(uint64)
		, ReasonStore64
		, ReasonStore32>::type            ReasonVec;
	typedef ReasonVec::value_type       ReasonWithData;
	Assignment() : front(0), elims_(0), units_(0) { }
	LitVec            trail;   // assignment sequence
	LitVec::size_type front;   // and "propagation queue"
	bool              qEmpty() const { return front == trail.size(); }
	uint32            qSize()  const { return (uint32)trail.size() - front; }
	Literal           qPop()         { return trail[front++]; }
	void              qReset()       { front  = trail.size(); }

	//! Number of variables in the three-valued assignment.
	uint32            numVars()    const { return (uint32)assign_.size(); }
	//! Number of assigned variables.
	uint32            assigned()   const { return (uint32)trail.size();   }
	//! Number of free variables.
	uint32            free()       const { return numVars() - (assigned()+elims_);   }
	//! Returns the largest possible decision level.
	uint32            maxLevel()   const { return (1u<<28)-2; }
	//! Returns v's value in the three-valued assignment.
	ValueRep          value(Var v) const { return ValueRep(assign_[v] & 3u); }
	//! Returns the decision level on which v was assigned if value(v) != value_free.
	uint32            level(Var v) const { return assign_[v] >> 4u; }
	//! Returns true if v was not eliminated from the assignment.
	bool              valid(Var v) const { return (assign_[v] & elim_mask) != elim_mask; }
	//! Returns the set of preferred values of v.
	const ValueSet    pref(Var v)  const { return v < pref_.size() ? pref_[v] : ValueSet(); }
	//! Returns the reason for v being assigned if value(v) != value_free.
	const Antecedent& reason(Var v)const { return reason_[v]; }
	//! Returns the number of allocated data slots.
	uint32            numData()    const { return reason_.dataSize(); }
	//! Returns the reason data associated with v.
	uint32            data(Var v)  const { assert(v < reason_.dataSize()); return reason_.data(v); }

	//! Resize to nv variables.
	void resize(uint32 nv) {
		assign_.resize(nv);
		reason_.resize(nv);
	}
	//! Adds a var to assignment - initially the new var is unassigned.
	Var addVar() {
		assign_.push_back(0);
		reason_.push_back(0);
		return numVars()-1;
	}
	//! Allocates space for storing preferred values for all variables.
	void requestPrefs() {
		if (pref_.size() != assign_.size()) { pref_.resize(assign_.size()); }
	}
	//! Allocates data slots for nv variables to be used for storing additional reason data.
	void requestData(uint32 nv) {
		reason_.dataResize(nv);
	}
	//! Eliminates v from the assignment.
	void eliminate(Var v) {
		assert(value(v) == value_free && "Can not eliminate assigned var!\n");
		if (valid(v)) { assign_[v] = elim_mask|value_true; ++elims_; }
	}
	//! Assigns p.var() on level lev to the value that makes p true and stores x as reason for the assignment.
	/*!
	 * \return true if the assignment is consistent. False, otherwise.
	 * \post If true is returned, p is in trail. Otherwise, ~p is.
	 */
	bool assign(Literal p, uint32 lev, const Antecedent& x) {
		const Var      v   = p.var();
		const ValueRep val = value(v);
		if (val == value_free) {
			assert(valid(v));
			assign_[v] = (lev<<4) + trueValue(p);
			reason_[v] = x;
			trail.push_back(p);
			return true;
		}
		return val == trueValue(p);
	}
	bool assign(Literal p, uint32 lev, Constraint* c, uint32 data) {
		const Var      v   = p.var();
		const ValueRep val = value(v);
		if (val == value_free) {
			assert(valid(v));
			assign_[v] = (lev<<4) + trueValue(p);
			reason_[v] = c;
			reason_.setData(v, data);
			trail.push_back(p);
			return true;
		}
		return val == trueValue(p);
	}
	//! Undos all assignments in the range trail[first, last).
	/*!
	 * \param first First assignment to be undone.
	 * \param save  If true, previous assignment of a var is saved before it is undone.
	 */
	void undoTrail(LitVec::size_type first, bool save) {
		if (!save) { popUntil<&Assignment::clear>(trail[first]); }
		else       { requestPrefs(); popUntil<&Assignment::saveAndClear>(trail[first]); }
		qReset();
	}
	//! Undos the last assignment.
	void undoLast() { clear(trail.back().var()); trail.pop_back(); }
	//! Returns the last assignment as a true literal.
	Literal last() const { return trail.back(); }
	Literal&last()       { return trail.back(); }
	/*!
	 * \name Implementation functions
	 * Low-level implementation functions. Use with care and only if you
	 * know what you are doing!
	 */
	//@{
	bool seen(Var v, uint8 m) const { return (assign_[v] & (m<<2)) != 0; }
	void setSeen(Var v, uint8 m)    { assign_[v] |= (m<<2); }
	void clearSeen(Var v)           { assign_[v] &= ~uint32(12); }
	void clearValue(Var v)          { assign_[v] &= ~uint32(3); }
	void setValue(Var v, ValueRep val) {
		assert(value(v) == val || value(v) == value_free);
		assign_[v] |= val;
	}
	void setReason(Var v, const Antecedent& a) { reason_[v] = a;  }
	void setData(Var v, uint32 data) { reason_.setData(v, data); }
	void setPref(Var v, ValueSet::Value which, ValueRep to) { pref_[v].set(which, to); }
	void copyAssignment(Assignment& o) const { o.assign_ = assign_; }
	bool markUnits()                 { while (units_ != front) { setSeen(trail[units_++].var(), 3u); }  return true; }
	//@}
private:
	static const uint32 elim_mask = uint32(0xFFFFFFF0u);
	Assignment(const Assignment&);
	Assignment& operator=(const Assignment&);
	void    clear(Var v)              { assign_[v]= 0; }
	void    saveAndClear(Var v)       { pref_[v].save(value(v)); clear(v); }
	template <void (Assignment::*op)(Var v)>
	void popUntil(Literal stop) {
		Literal p;
		do {
			p = trail.back(); trail.pop_back();
			(this->*op)(p.var());
		} while (p != stop);
	}
	AssignVec assign_; // for each var: three-valued assignment
	ReasonVec reason_; // for each var: reason for being assigned (+ optional data)
	PrefVec   pref_;   // for each var: set of preferred values
	uint32    elims_;  // number of variables that were eliminated from the assignment
	uint32    units_;  // number of marked top-level assignments
};

//! Stores information about a literal that is implied on an earlier level than the current decision level.
struct ImpliedLiteral {
	typedef Assignment::ReasonWithData AnteInfo;
	ImpliedLiteral(Literal a_lit, uint32 a_level, const Antecedent& a_ante, uint32 a_data = UINT32_MAX) 
		: lit(a_lit)
		, level(a_level)
		, ante(a_ante, a_data) {
	}
	Literal     lit;    /**< The implied literal */
	uint32      level;  /**< The earliest decision level on which lit is implied */
	AnteInfo    ante;   /**< The reason why lit is implied on decision-level level */
};
//! A type for storing ImpliedLiteral objects.
struct ImpliedList {
	typedef PodVector<ImpliedLiteral>::type VecType;
	typedef VecType::const_iterator iterator;
	ImpliedList() : level(0), front(0) {}
	//! Searches for an entry <p> in list. Returns 0 if none is found.
	ImpliedLiteral* find(Literal p) {
		for (uint32 i = 0, end = lits.size(); i != end; ++i) {
			if (lits[i].lit == p)  { return &lits[i]; }
		}
		return 0;
	}
	//! Adds a new object to the list.
	void add(uint32 dl, const ImpliedLiteral& n) {
		if (dl > level) { level = dl; }
		lits.push_back(n);
	}
	//! Returns true if list contains entries that must be reassigned on current dl.
	bool active(uint32 dl) const { return dl < level && front != lits.size(); }
	//! Reassigns all literals that are still implied.
	bool assign(Solver& s);
	iterator begin() const { return lits.begin(); }
	iterator end()   const { return lits.end();   }
	VecType lits;  // current set of (out-of-order) implied literals
	uint32  level; // highest dl on which lits must be reassigned
	uint32  front; // current starting position in lits
};

struct CCMinRecursive {
	enum State { state_open = 0, state_poison = 1, state_removable = 2 };
	void  init(uint32 numV) { extra.resize(numV,0); }
	State state(Literal p) const { return State(extra[p.var()]); }
	bool  checkRecursive(Literal p) {
		if (state(p) == state_open) { p.clearWatch(); dfsStack.push_back(p); }
		return state(p) != state_poison;
	}
	void  markVisited(Literal p, State st) {
		if (state(p) == state_open) {
			visited.push_back(p.var());
		}
		extra[p.var()] = static_cast<uint8>(st);
	}
	void clear() {
		for (; !visited.empty(); visited.pop_back()) {
			extra[visited.back()] = 0;
		}
	}
	typedef PodVector<uint8>::type DfsState;
	LitVec   dfsStack;
	VarVec   visited;
	DfsState extra;
};
//@}
}
#endif
