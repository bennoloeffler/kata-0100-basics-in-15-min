/*
 * Decompiled with CFR 0.152.
 * 
 * Could not load the following classes:
 *  clojure.lang.ASeq
 *  clojure.lang.IDeref
 *  clojure.lang.IPending
 *  clojure.lang.IPersistentMap
 *  clojure.lang.IReduce
 *  clojure.lang.ISeq
 *  clojure.lang.RT
 *  java.lang.Object
 */
package clojure.lang;

import clojure.lang.ASeq;
import clojure.lang.IDeref;
import clojure.lang.IFn;
import clojure.lang.IPending;
import clojure.lang.IPersistentMap;
import clojure.lang.IReduce;
import clojure.lang.ISeq;
import clojure.lang.RT;

public class Iterate
extends ASeq
implements IReduce,
IPending {
    private static final Object UNREALIZED_SEED = new Object();
    private final IFn f;
    private final Object prevSeed;
    private volatile Object _seed;
    private volatile ISeq _next;

    private Iterate(IFn f, Object prevSeed, Object seed) {
        this.f = f;
        this.prevSeed = prevSeed;
        this._seed = seed;
    }

    private Iterate(IPersistentMap meta, IFn f, Object prevSeed, Object seed, ISeq next) {
        super(meta);
        this.f = f;
        this.prevSeed = prevSeed;
        this._seed = seed;
        this._next = next;
    }

    public static ISeq create(IFn f, Object seed) {
        return new Iterate(f, null, seed);
    }

    public boolean isRealized() {
        return this._seed != UNREALIZED_SEED;
    }

    public Object first() {
        if (this._seed == UNREALIZED_SEED) {
            this._seed = this.f.invoke(this.prevSeed);
        }
        return this._seed;
    }

    public ISeq next() {
        if (this._next == null) {
            this._next = new Iterate(this.f, this.first(), UNREALIZED_SEED);
        }
        return this._next;
    }

    public Iterate withMeta(IPersistentMap meta) {
        if (this.meta() == meta) {
            return this;
        }
        return new Iterate(meta, this.f, this.prevSeed, this._seed, this._next);
    }

    public Object reduce(IFn rf) {
        Object first;
        Object ret = first = this.first();
        Object v = this.f.invoke(first);
        while (!RT.isReduced((Object)(ret = rf.invoke(ret, v)))) {
            v = this.f.invoke(v);
        }
        return ((IDeref)ret).deref();
    }

    public Object reduce(IFn rf, Object start) {
        Object ret = start;
        Object v = this.first();
        while (!RT.isReduced((Object)(ret = rf.invoke(ret, v)))) {
            v = this.f.invoke(v);
        }
        return ((IDeref)ret).deref();
    }
}
