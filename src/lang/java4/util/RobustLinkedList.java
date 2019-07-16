/* Copyright (c) 2013, Jesper Öqvist <jesper.oqvist@cs.lth.se>
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
import java.util.Iterator;

/**
 * A robust linked list that can be iterated while elements are added.
 *
 * <p>The list supports concurrent modification and iteration in multiple
 * modifier/iterator threads. Thread safety is achieved by mutual exclusion
 * using the list instance as a lock.
 *
 * <p>The robust linked list always keeps a tail node, which the iterator
 * points to when it has run past the end.
 *
 * @author Jesper Öqvist <jesper.oqvist@cs.lth.se>
 * @param <V> The list item type
 */
public class RobustLinkedList<V> implements Collection<V> {

  private static final class Node<V> {
    public V datum;
    Node<V> succ = null;
    Node<V> pred = null;

    public Node(V v) {
      this.datum = v;
    }
  }

  /**
   * A robust linked list iterator.
   * @author Jesper Öqvist <jesper.oqvist@cs.lth.se>
   * @param <I> Value type of the iterator.
   */
  public class RobustIterator<I> implements Iterator<I> {

    private Node<I> ptr;

    /**
     * Build a robust iterator for the list.
     * @param head
     */
    public RobustIterator(Node<I> head) {
      ptr = new Node<I>(null);
      ptr.succ = head;
    }

    @Override
    public boolean hasNext() {
      boolean result;
      synchronized (RobustLinkedList.this) {
        result = ptr.succ.succ != null;
      }
      return result;
    }

    @Override
    public I next() {
      I datum;
      synchronized (RobustLinkedList.this) {
        datum = ptr.succ.datum;
        ptr.succ = ptr.succ.succ;
      }
      return datum;
    }

    @Override
    public void remove() {
      synchronized (RobustLinkedList.this) {
        Node<I> pred = ptr.succ.pred;
        Node<I> succ = ptr.succ.succ;
        if (pred != null) {
          pred.succ = succ;
        }
        if (succ != null) {
          succ.pred = pred;
          size -= 1;
        }
        ptr.succ = succ;
      }
    }
  }

  /** Sentinel empty tail node. Not part of the list. */
  private Node<V> tail = new Node<V>(null);

  /** Head node. Points to tail node if list is empty. */
  private Node<V> head = tail;

  private int size;

  @Override
  public synchronized boolean add(V v) {
    tail.datum = v;
    Node<V> node = new Node<V>(null);
    node.pred = tail;
    tail.succ = node;
    tail = node;
    size += 1;
    return true;
  }

  @Override
  public synchronized boolean addAll(Collection<? extends V> collection) {
    boolean changed = false;
    for (V v : collection) {
      add(v);
      changed = true;
    }
    return changed;
  }

  @Override
  public synchronized void clear() {
    // Set all successor pointers to null.
    Node<V> ptr = head;
    while (ptr != null) {
      Node<V> succ = ptr.succ;
      ptr.succ = null;
      ptr = succ;
    }
    head = tail;
    size = 0;
  }

  @Override
  public synchronized boolean contains(Object o) {
    Iterator<V> iter = iterator();
    while (iter.hasNext()) {
      if (iter.next().equals(o)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public synchronized boolean containsAll(Collection<?> collection) {
    throw new UnsupportedOperationException();
  }

  @Override
  public synchronized boolean isEmpty() {
    return size == 0;
  }

  @Override
  public synchronized Iterator<V> iterator() {
    return new RobustIterator<V>(head);
  }

  @Override
  public synchronized boolean remove(Object o) {
    Iterator<V> iter = iterator();
    while (iter.hasNext()) {
      if (iter.next().equals(o)) {
        iter.remove();
        return true;
      }
    }
    return false;
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
    return size;
  }

  @Override
  public Object[] toArray() {
    return null;
  }

  @Override
  public <T> T[] toArray(T[] array) {
    return null;
  }
}
