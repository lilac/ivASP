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

#ifndef _GRINGO_UNIQUE_LIST_HH
#define _GRINGO_UNIQUE_LIST_HH

#include <unordered_map>

namespace Gringo {

// {{{ declaration of unique_list

template <class Key, class T, class Hash = std::hash<Key>, class Pred = std::equal_to<Key>>
struct unique_list {
    struct node;
    struct iterator;
    struct const_iterator;
    using key_type    = Key;
    using mapped_type = T;
    using value_type  = std::pair<key_type const, node>;
    struct node {
        friend struct iterator;
        friend struct const_iterator;
        friend struct unique_list;
        node() { }
        template <class... Args>
        node(Args&&... x) : _value(std::forward<Args>(x)...) { }
        mapped_type const &operator*() const  { return _value; }
        mapped_type &operator*()              { return _value; }
        mapped_type const *operator->() const { return &_value; }
        mapped_type *operator->()             { return &_value; }
    private:
        mapped_type  _value;
        value_type  *_left;
        value_type  *_right;
    };
    using hasher          = Hash;
    using key_equal       = Pred;
    using map_type        = std::unordered_map<key_type, node, hasher, key_equal>;
    struct const_iterator;
    struct iterator {
        friend struct unique_list;
        iterator() : _n(nullptr) { }
        iterator(iterator const &x) = default;
        bool operator==(const_iterator const &x) const;
        bool operator!=(const_iterator const &x) const;
        bool operator==(iterator const &x) const       { return _n == x._n; }
        bool operator!=(iterator const &x) const       { return _n != x._n; }
        value_type &operator*()                        { return *_n; }
        value_type *operator->()                       { return _n; }
        iterator &operator++() {
            _n = _n->second._right;
            return *this;
        }
    private:
        iterator(value_type *x) : _n(x) { }
        value_type *_n;
    };
    struct const_iterator {
        friend struct unique_list;
        const_iterator() : _n(nullptr) { }
        const_iterator(const_iterator const &x) = default;
        const_iterator(iterator const &x) : _n(x._n)   { };
        bool operator==(iterator const &x) const       { return _n == x._n; }
        bool operator!=(iterator const &x) const       { return _n != x._n; }
        bool operator==(const_iterator const &x) const { return _n == x._n; }
        bool operator!=(const_iterator const &x) const { return _n != x._n; }
        value_type const &operator*()                  { return *_n; }
        value_type const *operator->()                 { return _n; }
        const_iterator &operator++() {
            _n = _n->second._right;
            return *this;
        }
    private:
        const_iterator(value_type const *x) : _n(x) { }
        value_type const *_n;
    };
    using size_type       = typename map_type::size_type;
    using difference_type = typename map_type::difference_type;

    unique_list() = default;
    unique_list(unique_list const &) = default;
    unique_list(unique_list &&) = default;

    bool empty() const         { return _map.empty(); }
    size_type size() const     { return _map.size(); }
    size_type max_size() const { return _map.max_size(); }

    value_type &front()          { return *_front; }
    value_type &back()           { return *_back; }
    iterator begin()             { return _front; }
    const_iterator begin() const { return _front; }
    iterator end()               { return nullptr; }
    const_iterator end() const   { return nullptr; }

    iterator find(const key_type &x) { 
        auto it(_map.find(x));
        return iterator(it != _map.end() ? &*it : nullptr);
    }
    const_iterator find (const key_type &x) const { 
        auto it(_map.find(x));
        return const_iterator(it != _map.end() ? &*it : nullptr);
    }

    template <class... Args> 
    std::pair<iterator, bool> emplace_back(Args&&... args) {
        auto ret(_map.emplace(std::forward<Args>(args)...));
        if (ret.second) {
            if (_back) {
                ret.first->second._left     = _back;
                ret.first->second._right    = nullptr;
                _back->second._right        = &*ret.first;
                _back                       = &*ret.first;
            }
            else {
                ret.first->second._left     = nullptr;
                ret.first->second._right    = nullptr;
                _back  = &*ret.first;
                _front = _back;
            }
        }
        return {iterator(&*ret.first), ret.second};
    }
    iterator erase(const_iterator x) {
        if (x->second._left) { x->second._left->second._right = x->second._right; }
        else                 { _front = x->second._right; }
        if (x->second._right) { x->second._right->second._left = x->second._left; }
        else                  { _back = x->second._left; }
        auto next(x->second._right);
        _map.erase(x->first);
        return next;
    }
    size_type erase(key_type const &x) {
        auto it(_map.find(x));
        if (it != _map.end()) {
            erase(&*it);
            return 1;
        }
        else { return 0; }
    }
    iterator erase(const_iterator a, const_iterator b) {
        if (a != b) {
            auto n(const_cast<value_type*>(b._n));
            if (a->second._left) { a->second._left->second._right = n; }
            else                 { _front = n; }
            if (b._n) { n->second._left = a->second._left; }
            else      { _back = a->second._left; }
            while (a != b) { 
                auto c(a);
                ++a;
                _map.erase(c->first);
            }
        }
        return const_cast<value_type*>(b._n);
    }
    void clear() {
        _front = nullptr;
        _back  = nullptr;
        _map.clear();
    }
    void swap (unique_list &x) {
        std::swap(_front, x._front);
        std::swap(_back, x._back);
        _map.swap(x._map);
    }
private:
    map_type    _map;
    value_type *_front = nullptr;
    value_type *_back  = nullptr;
};

template <class Key, class T, class Hash, class Pred>
bool unique_list<Key, T, Hash, Pred>::iterator::operator==(unique_list::const_iterator const &x) const { return x == *this; }
template <class Key, class T, class Hash, class Pred>
bool unique_list<Key, T, Hash, Pred>::iterator::operator!=(unique_list::const_iterator const &x) const { return x != *this; }

// }}}

} // namespace Gringo

#endif // _GRINGO_UNIQUE_LIST_HH


