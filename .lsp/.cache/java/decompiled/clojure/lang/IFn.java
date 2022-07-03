/*
 * Decompiled with CFR 0.152.
 * 
 * Could not load the following classes:
 *  clojure.lang.ISeq
 *  java.lang.Object
 *  java.lang.Runnable
 *  java.util.concurrent.Callable
 */
package clojure.lang;

import clojure.lang.ISeq;
import java.util.concurrent.Callable;

public interface IFn
extends Callable,
Runnable {
    public Object invoke();

    public Object invoke(Object var1);

    public Object invoke(Object var1, Object var2);

    public Object invoke(Object var1, Object var2, Object var3);

    public Object invoke(Object var1, Object var2, Object var3, Object var4);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12, Object var13);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12, Object var13, Object var14);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12, Object var13, Object var14, Object var15);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12, Object var13, Object var14, Object var15, Object var16);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12, Object var13, Object var14, Object var15, Object var16, Object var17);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12, Object var13, Object var14, Object var15, Object var16, Object var17, Object var18);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12, Object var13, Object var14, Object var15, Object var16, Object var17, Object var18, Object var19);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12, Object var13, Object var14, Object var15, Object var16, Object var17, Object var18, Object var19, Object var20);

    public Object invoke(Object var1, Object var2, Object var3, Object var4, Object var5, Object var6, Object var7, Object var8, Object var9, Object var10, Object var11, Object var12, Object var13, Object var14, Object var15, Object var16, Object var17, Object var18, Object var19, Object var20, Object ... var21);

    public Object applyTo(ISeq var1);
}
