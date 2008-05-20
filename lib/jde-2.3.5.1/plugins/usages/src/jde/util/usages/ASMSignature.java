/*
 * $Id: ASMSignature.java,v 1.6 2006/04/07 22:44:46 surajacharya Exp $
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

import jde.util.Usages;
import org.objectweb.asm.Type;

public class ASMSignature implements ASMUsages.Signature, Usages.Signature {

    Usages.Signature _m;
    ASMUsages.Signature _am;

    public ASMSignature (Usages.Signature m) {
        _m = m;
    }

    public ASMSignature (ASMUsages.Signature am) {
        _am = am;
    }

    public boolean matches (String methodName, String retType, String[] argTypes, boolean isField) {
        if (_m!= null) // delegate
            return _m.matches (methodName, retType, argTypes, isField);
 
        // adapt, we don't use this so it's not implemented for now.
        throw new RuntimeException ("Adaptor from ASMUsages.Signature to Usages.Signature not implemented");
    }

    public boolean matches (String methodName, Type retType, Type[] argTypes, boolean isField) {
        if (_am != null) //delegate
            return _am.matches (methodName, retType, argTypes, isField);

        // adapt
        String args[] = new String [argTypes.length];

        for (int i = 0; i < argTypes.length; i++) {
            args[i] = Usages.getTypeString (argTypes[i]);
        }        
        
        return matches (methodName, Usages.getTypeString (retType), args, isField);

    }


    public boolean isConstructor () {
        if (_am != null)
            return _am.isConstructor();
        else
            return _m.isConstructor();
    }

    public ASMSignature (final String methodName, final String[] _args, final boolean _isField) {
        this (new Usages.Signature () {
                public boolean matches (String m, String r, String[] args, boolean isField) {
                    if (isField == _isField && methodName.equals(m) && _args.length == args.length) {
                        for (int i = 0 ; i < _args.length;i++)
                            if (!args[i].endsWith (_args[i]))
                                return false;
                        return true;
                    } else
                        return false;
                }

                public boolean isConstructor () {
                    return methodName.equals ("<init>");
                }

            });
        // _args has class names in the foo.bar.ClassName format, while args has
        // class names from the class files, which will look like foo/bar/ClassName
        for (int i = 0 ; i < _args.length ; i ++) {
            _args[i] = _args[i].replace ('.', '/');
        }
    }

    public ASMSignature (final String methodName, final int numParams, final boolean _isField) {
        this (new Usages.Signature () {
                public boolean matches (String m, String r, String[] args, boolean isField) {
                    if (_isField == isField && methodName.equals(m) && numParams == args.length){
                        return true;
                    } else
                        return false;
                }

                public boolean isConstructor () {
                    return methodName.equals ("<init>");
                }

            });
    }

    public static final ASMSignature MATCH_ALL_MEMBERS = new ASMSignature ( new Usages.Signature() {
            public boolean matches (String m, String r, String[] args, boolean isField) { return true; }
            public boolean isConstructor () { return true; }
        });
    
}
