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

#ifndef _GRINGO_INTERVALS_HH
#define _GRINGO_INTERVALS_HH

#include <vector>
#include <iostream>
#include <algorithm>

namespace Gringo {

template <class T>
struct IntervalSet {
    // Notes: read < as before with gap (except if bounds of same type are compared)
    using Value = T;
    struct RBound;
    struct LBound {
        LBound &operator=(RBound const &x) {
            bound     = x.bound;
            inclusive = !x.inclusive;
            return *this;
        }
        Value bound;
        bool inclusive;
        bool operator<(LBound const &x) const  { return bound < x.bound || (!(x.bound < bound) &&  inclusive && !x.inclusive ); }
        bool operator<=(LBound const &x) const { return bound < x.bound || (!(x.bound < bound) && (inclusive || !x.inclusive)); }
        bool operator<(RBound const &x) const  { return bound < x.bound || (!(x.bound < bound) &&  inclusive &&  x.inclusive ); }
    };
    struct RBound {
        RBound &operator=(LBound const &x) {
            bound     = x.bound;
            inclusive = !x.inclusive;
            return *this;
        }
        Value bound;
        bool inclusive;
        bool operator<(RBound const &x) const  { return bound < x.bound || (!(x.bound < bound) &&  !inclusive &&  x.inclusive ); }
        bool operator<=(RBound const &x) const { return bound < x.bound || (!(x.bound < bound) && (!inclusive ||  x.inclusive)); }
        bool operator<(LBound const &x) const  { return bound < x.bound || (!(x.bound < bound) &&  !inclusive && !x.inclusive ); }
    };
    struct Interval {
        bool contains(T const &x) const         { return !(x < *this) && !(*this < x); }
        bool empty() const                      { return !(left < right); }
        bool operator<(Interval const &x) const { return right < x.left; }
        LBound left;
        RBound right;
    };
    using IntervalVec = std::vector<Interval>;
    using const_iterator = typename IntervalVec::const_iterator;

    void add(Interval const &x) {
        if (!x.empty()) {
            auto it = std::lower_bound(vec.begin(), vec.end(), x);
            if (it == vec.end()) { vec.emplace_back(x); }
            else {
                auto jt = std::upper_bound(it, vec.end(), x);
                if (it == jt) { vec.emplace(it, x); }
                else {
                    it->left  = std::min(x.left, it->left);
                    it->right = std::max(x.right, (jt-1)->right);
                    vec.erase(it+1, jt);
                }
            }
        }
    }
    void remove(Interval const &x) {
        if (!x.empty()) {
            auto it = std::lower_bound(vec.begin(), vec.end(), x);
            if (it != vec.end()) {
                auto jt = std::upper_bound(it, vec.end(), x);
                if (it+1 == jt) {
                    Interval r;
                    r.left    = x.right;
                    r.right   = it->right;
                    it->right = x.left;
                    if (it->empty()) {
                        if (r.empty()) { vec.erase(it); }
                        else           { *it = r; }
                    }
                    else if (!r.empty()) { vec.emplace(it+1, r); }
                }
                else if (it != jt) {
                    it->right    = x.left;
                    (jt-1)->left = x.right;
                    vec.erase(it + !it->empty(), jt - !(jt-1)->empty());
                }
            }
        }
    }
    bool contains(Interval const &x) const { 
        if (x.empty()) { return true; }
        for (auto &y : vec) {
            if (x.right <= y.right) { return y.left <= x.left; }
        }
        return false;
    }
    bool intersects(Interval const &x) const { 
        if (!x.empty()) {
            for (auto &y : vec) {
                if (x.left < y.right) { return y.left < x.right; }
            }
        }
        return false;
    }
    bool empty() const { return vec.empty(); }
    void clear() { return vec.clear(); }
    void add(Value const &a, bool ta, Value const &b, bool tb) { return add({{a, ta}, {b, tb}}); }
    const_iterator begin() const { return vec.begin(); }
    const_iterator end() const { return vec.end(); }
    void remove(Value const &a, bool ta, Value const &b, bool tb) { return remove({{a, ta}, {b, tb}}); }

    IntervalVec vec;
};

template <class T>
bool operator<(T const &a, typename IntervalSet<T>::Interval const &b) {
    return a < b.left.bound || (!(b.left.bound < a) && !b.left.inclusive);
}

template <class T>
bool operator<(typename IntervalSet<T>::Interval const &a, T const &b) {
    return a.right.bound < b || (!(b < a.right.bound) && !a.right.inclusive);
}

} // namespace Gringo

namespace std {

template <class T>
typename Gringo::IntervalSet<T>::const_iterator begin(Gringo::IntervalSet<T> const &x) { return x.begin(); }

template <class T>
typename Gringo::IntervalSet<T>::const_iterator end(Gringo::IntervalSet<T> const &x) { return x.end(); }

} // namespace std

#endif // _GRINGO_INTERVALS_HH
