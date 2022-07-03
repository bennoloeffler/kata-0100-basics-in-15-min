/*
 * Decompiled with CFR 0.152.
 * 
 * Could not load the following classes:
 *  clojure.lang.IFn
 *  clojure.lang.ISeq
 *  java.lang.Object
 */
package clojure.lang;

import clojure.lang.IFn;
import clojure.lang.ISeq;

public interface IAtom {
    public Object swap(IFn var1);

    public Object swap(IFn var1, Object var2);

    public Object swap(IFn var1, Object var2, Object var3);

    public Object swap(IFn var1, Object var2, Object var3, ISeq var4);

    public boolean compareAndSet(Object var1, Object var2);

    public Object reset(Object var1);
}
