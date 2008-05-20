package jde.util.usages;

/**
 * This class tries to provide an abstract representation of a fully qualified
 * java class name. It uses interned strings right now but the goal is use MD5
 * hashes as an attempt to reduce the size of cache files. Since
 * <code>makeClassName</code> is not reversible, creating a ClassPathEntry from
 * a cached file would also require getting a list of classes from the jar or
 * directory as this list is not present in the cache.
 *
 */
public class ClassName {

    private static ClassName.I implementation = new InternedString();
    
    public static Object makeClassName (String classname) {
        return implementation.makeClassName (classname);
    }

    public static String getClassNameString (Object classname) {
        return implementation.getClassNameString (classname);
    }

    private interface I {
        public Object makeClassName (String classname);
        
        public String getClassNameString (Object classname);

    }

    private static class InternedString implements I {
        public Object makeClassName (String classname) {
            return classname.intern();
        }

        public String getClassNameString (Object classname) {
            return (String) classname;
        }

    }

    private static class HashInt implements I {
        public Object makeClassName (String classname) {
            return new Integer (classname.hashCode()).toString().intern();
        }

        public String getClassNameString (Object classname) {
            return null;
        }
        
    }

    private static class MD5 implements I {

        public Object makeClassName (String classname) {
            return MD5Impl.getMD5hash (classname);
        }
        
        public String getClassNameString (Object classname) {
            return null;
        }

    
        static private class MD5Impl {
            byte [] md5;

            static java.security.MessageDigest MD5;

            static {
                try {
                    MD5 = java.security.MessageDigest.getInstance ("MD5");
                } catch (java.security.NoSuchAlgorithmException e) {MD5 = null;}
            }
    
            static String getMD5hash (String str) {
                byte[] b;
                try {
                    b = str.getBytes("UTF8");
                } catch (java.io.UnsupportedEncodingException e) {
                    b = str.getBytes();
                    // FIXME: is this wise? don't we depend on UTF8?
                }
                return getMD5hash (b, 0, b.length);
            }


            static String getMD5hash (byte[] b, int off, int len) {
                if (MD5 == null)
                    if (off == 0 && len == b.length)
                        return encodeHash (b);
                    else {
                        byte[] bCopy = new byte[len];
                        System.arraycopy (b, off, bCopy, 0, len);
                        return encodeHash (bCopy);
                    }
            
                MD5.reset();
                MD5.update (b, off, len);
                return encodeHash (MD5.digest());
            }

            static String encodeHash (byte[] hash){
                StringBuffer sb = new StringBuffer();
                for (int i=0; i < hash.length;i++)
                    sb.append ((char) hash[i]);
                return sb.toString().intern();
            }
        
        }
        
    }
    
    

}