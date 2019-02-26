{ mkDerivation, async, base, hspec, lens, mtl, pipes, pqueue, process
, QuickCheck, transformers, unagi-chan, unix, uuid-types, vector-space}:
mkDerivation {
    pname = "adrenaline";
    version = "0.0.0.0";
    license = null;
    src = builtins.filterSource (p: t: p != toString ./dist &&
                                       p != toString ./result)
                                ./.;
    buildDepends = [
        async
        base
        hspec
        lens
        mtl
        pipes
        pqueue
        process
        QuickCheck
        transformers
        unagi-chan
        unix
        uuid-types
        vector-space
    ];
}
