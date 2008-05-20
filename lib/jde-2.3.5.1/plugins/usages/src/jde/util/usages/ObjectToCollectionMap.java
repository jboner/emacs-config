/*
 * $Id: ObjectToCollectionMap.java,v 1.3 2004/09/24 00:37:53 surajacharya Exp $
 * Copyright (C) 2004 Suraj Acharya (sacharya@cs.indiana.edu)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

package jde.util.usages;


import java.util.Map;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.LinkedList;


public class ObjectToCollectionMap implements Map, java.io.Serializable {

    private Map _map;
    private Class _collectionType;

    // Code for delegation of java.util.Map methods to map

    /**
     * Describe <code>hashCode</code> method here.
     *
     * @return an <code>int</code> value
     */
    public int hashCode() {
        return _map.hashCode();
    }

    /**
     * Describe <code>put</code> method here.
     *
     * @param object an <code>Object</code> value
     * @param object1 an <code>Object</code> value
     * @return an <code>Object</code> value
     */
    public Object put(Object object, Object object1) {
        return _map.put(object, object1);
    }

    /**
     * Describe <code>equals</code> method here.
     *
     * @param object an <code>Object</code> value
     * @return a <code>boolean</code> value
     */
    public boolean equals(Object object) {
        return _map.equals(object);
    }

    /**
     * Describe <code>get</code> method here.
     *
     * @param object an <code>Object</code> value
     * @return an <code>Object</code> value
     */
    public Object get(Object object) {
        return _map.get(object);
    }

    /**
     * Describe <code>size</code> method here.
     *
     * @return an <code>int</code> value
     */
    public int size() {
        return _map.size();
    }

    /**
     * Describe <code>remove</code> method here.
     *
     * @param object an <code>Object</code> value
     * @return an <code>Object</code> value
     */
    public Object remove(Object object) {
        return _map.remove(object);
    }

    /**
     * Describe <code>values</code> method here.
     *
     * @return a <code>Collection</code> value
     */
    public Collection values() {
        return _map.values();
    }

    /**
     * Describe <code>clear</code> method here.
     *
     */
    public void clear() {
        _map.clear();
    }

    /**
     * Describe <code>keySet</code> method here.
     *
     * @return a <code>Set</code> value
     */
    public Set keySet() {
        return _map.keySet();
    }

    /**
     * Describe <code>entrySet</code> method here.
     *
     * @return a <code>Set</code> value
     */
    public Set entrySet() {
        return _map.entrySet();
    }

    /**
     * Describe <code>isEmpty</code> method here.
     *
     * @return a <code>boolean</code> value
     */
    public boolean isEmpty() {
        return _map.isEmpty();
    }

    /**
     * Describe <code>containsValue</code> method here.
     *
     * @param object an <code>Object</code> value
     * @return a <code>boolean</code> value
     */
    public boolean containsValue(Object object) {
        return _map.containsValue(object);
    }

    /**
     * Describe <code>containsKey</code> method here.
     *
     * @param object an <code>Object</code> value
     * @return a <code>boolean</code> value
     */
    public boolean containsKey(Object object) {
        return _map.containsKey(object);
    }

    /**
     * Describe <code>putAll</code> method here.
     *
     * @param map a <code>Map</code> value
     */
    public void putAll(Map map) {
        _map.putAll(map);
    }
    
    public ObjectToCollectionMap () {
        this (LinkedList.class);
    }
    
    public ObjectToCollectionMap (Class collectionType) {
        if (!Collection.class.isAssignableFrom(collectionType))
            throw new RuntimeException ("collectionType is not a java.util.Collection");
        _map = new HashMap();
        _collectionType = collectionType;
    }
    
    public void addToCollection (Object key, Object object) {
        Collection c = (Collection) _map.get (key);
        if (c == null) {
            try {
                c = (Collection) _collectionType.newInstance();
            } catch (Exception e) {throw new RuntimeException (e);}
            _map.put (key, c);
        }
        c.add (object);
    }

    public void addAllToCollection (Object key, Collection col) {
        Collection c = (Collection) _map.get (key);
        if (c == null) {
            try {
                c = (Collection) _collectionType.newInstance();
            } catch (Exception e) {throw new RuntimeException (e);}
            _map.put (key, c);
        }
        c.addAll (col);
    }
    
    public void removeFromCollection (Object key, Object object) {
        Collection c = (Collection) _map.get (key);
        if (c != null) {
            c.remove(object);
        }
    }

    public Collection getC (Object key) {
        return (Collection) get(key);
    }
    

}
