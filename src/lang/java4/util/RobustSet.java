/* Copyright (c) 2015, Jesper Öqvist <jesper.oqvist@cs.lth.se>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package org.jastadd.util;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A set with robust iterators that can iterate over the set simultaneously
 * with it being mutated.
 *
 * <p>This set supports concurrent mutation during iteration.
 *
 * <p>Thread safety is achieved using mutual exclusion with the set instance as lock.
 *
 * <p>NB: It is not possible to remove elements from a robust set using the
 * iterator obtained via the method robustValueIterator!
 *
 * @author Jesper Öqvist <jesper.oqvist@cs.lth.se>
 * @param <V> Value type
 */
public class RobustSet<V> implements Set<V> {

  /** Underlying set. */
  private final Set<V> set;

  /** Sibling list used for robust iteration. */
  private final RobustLinkedList<V> list = new RobustLinkedList<V>();

  /**
   * Construct robust set based on a HashSet.
   */
  public RobustSet() {
    this(new HashSet<V>());
  }

  /**
   * Construct robust set as a wrapper around an underlying set.
   *
   * <p>The provided set may not be used after creating a robust set using it.
   * Doing so may cause concurrency problems and nullifies any thread safety
   * assumptions.
   *
   * @param underlyingSet
   */
  public RobustSet(Set<V> underlyingSet) {
    set = underlyingSet;
  }

  @Override
  public synchronized boolean add(V v) {
    if (set.add(v)) {
      list.add(v);
      return true;
    }
    return false;
  }

  @Override
  public synchronized boolean addAll(Collection<? extends V> collection) {
    boolean changed = false;
    for (V v : collection) {
      changed = add(v) || changed;
    }
    return changed;
  }

  @Override
  public synchronized void clear() {
    set.clear();
    list.clear();
  }

  @Override
  public synchronized boolean contains(Object o) {
    return set.contains(o);
  }

  @Override
  public synchronized boolean containsAll(Collection<?> collection) {
    throw new UnsupportedOperationException();
  }

  @Override
  public synchronized boolean isEmpty() {
    return set.isEmpty();
  }

  @Override
  public synchronized Iterator<V> iterator() {
    return list.iterator();
  }

  @Override
  public synchronized boolean remove(Object o) {
    list.remove(o);
    return set.remove(o);
  }

  @Override
  public synchronized boolean removeAll(Collection<?> collection) {
    boolean changed = false;
    for (Object o : collection) {
      changed = remove(o) || changed;
    }
    return changed;
  }

  @Override
  public boolean retainAll(Collection<?> collection) {
    throw new UnsupportedOperationException();
  }

  @Override
  public synchronized int size() {
    return set.size();
  }

  @Override
  public synchronized Object[] toArray() {
    return set.toArray();
  }

  @Override
  public synchronized <T> T[] toArray(T[] array) {
    return set.toArray(array);
  }
}
