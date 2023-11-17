class UnionFind {
    constructor(size) {
        this.par = [];
        for(let i = 0; i < size; i++) {
            this.par[i] = i;
        }
    }
    root(query) {
        if (this.par[query] == query) return query;  
        return this.par[query] = this.root(this.par[query]);
    }
    merge(x, y) {
        let rx = this.root(x), ry = this.root(y);
        if (rx == ry) return;
        this.par[ry] = rx;
    }
    same(x, y) {
        let rx = this.root(x), ry = this.root(y);
        return rx == ry;
    }
}
