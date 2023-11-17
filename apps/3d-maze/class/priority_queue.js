class PriorityQueue {
    constructor() {
        this.heap = [];
    }

    push(item) {
        let heap = this.heap;
        let i = heap.length++;
        let j;
        while(i) {
            j = Math.floor((i-1)/2);
            if (heap[j] <= item) break;
            heap[i] = heap[j];
            i = j;
        }
        heap[i] = item;
    }
    pop() {
        let heap = this.heap;
        let top = heap[0];
        let popped = heap.pop();

        let i = 0;
        let k = Math.floor(heap.length / 2);
        let j;

        while(i < k) {
            j = (i*2) + 1;
            if (j+1 < heap.length && heap[j+1] < heap[j]) ++j;
            if (popped <= heap[j]) break;
            heap[i] = heap[j];
            i = j;
        }
        if (heap.length) {
            heap[i] = popped;
        }
        return top;
    }

    size() {
        return this.heap.length;
    }
    top() {
        return this.heap[0];
    }
    isEmpty() {
        return this.heap.length == 0;
    }
}