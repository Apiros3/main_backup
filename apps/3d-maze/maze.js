"use strict";

var size_list = [];

const is_between = (value, lower, upper) => {
    return (lower <= value && value < upper);
}
const lis_between = (value, upper) => {
    for(let i = 0; i < value.length; i++) {
        if (!is_between(value[i],0,upper[i])) return false;
    }
    return true;
}
const flatten = ([x,y,z]) => {
    return x*size_list[1]*size_list[2] + y*size_list[2] + z;
} 

const create_maze = async ([x, y, z]) => {
    console.assert(1 <= x && x <= 30, x);
    console.assert(1 <= y && y <= 30, y);
    console.assert(1 <= z && z <= 30, z);
    const edges = [[1,0,0],[-1,0,0],[0,1,0],[0,-1,0],[0,0,1],[0,0,-1]];

    size_list = [x,y,z];
    const que = new PriorityQueue();

    for(let i = 0; i < x; i++) {
        for(let j = 0; j < y; j++) {
            for(let k = 0; k < z; k++) {
                for(let l = 0; l < edges.length; l++) {
                    let ti = i + edges[l][0], tj = j + edges[l][1], tk = k + edges[l][2];
                    if (lis_between([ti,tj,tk],size_list)) {
                        que.push([Math.random(), [i, j, k], [ti, tj, tk]]);
                    }
                }
            }
        }
    }
    const adjacent_list = {};
    const uf = new UnionFind(x*y*z);
    while(!que.isEmpty()) {
        let top = que.pop();
        let tx = flatten(top[1]), ty = flatten(top[2]);
        if (!uf.same(tx,ty)) {
            adjacent_list[[top[1],top[2]]] = true;
            uf.merge(tx,ty);
        }
    }
    return adjacent_list;
}

const draw_maze = async (adjacent_list, coord, stick) => {
    const stick_to_id = ["yz","xz","xy"];
    const stick_to_lis = [[1,2],[0,2],[0,1]];
    const edges = [[1,0],[-1,0],[0,1],[0,-1]];
    var canvas = document.getElementById(stick_to_id[stick]);
    var ctx = canvas.getContext("2d");
    const width = canvas.width, height = canvas.height;
    ctx.clearRect(0,0,width,height);

    var ti = stick_to_lis[stick][0], tj = stick_to_lis[stick][1];
    for(let i = 0; i < size_list[ti]; i++) {
        for(let j = 0;   j < size_list[tj]; j++) {
            for(let k = 0; k < edges.length; k++) {
                var tlis = [];
                tlis[ti] = i + edges[0];
                tlis[tj] = j + edges[1];
                tlis[stick] = coord[stick];

                // if (lis_between(tlis,size_list) && !(.concat(tlis) in adjacent_list)) {
                //     console.log()
                // }
            }
        }
    }



}