#include <bits/stdc++.h>
using namespace std;

// reference: https://www.kaggle.com/code/manzoormahmood/mnist-neural-network-from-scratch

using ll = long long;
using ull = unsigned long long;
using ld = long double;

#define rep(i,start,end) for(ll i=start; i<end; i++)
#define rrep(i,start,end) for(ll i=start; i>=end; i--)

ll INF = 1LL << 60;
ld PI = 3.141592653589793;

struct Init {
    Init() {
        cout << setprecision(15);
    }
}init;

pair<int,vector<int>> refine(string S) {
    int type = -1; 
    vector<int> ret;
    S += ",";
    string cur = "";
    for(auto u : S) {
        if (u == ',') {
            if (type == -1) {
                type = stoi(cur);
            }
            else {
                ret.push_back(stoi(cur));
            }
            cur = "";
        }
        else cur += u;
    }
    return {type,ret};
}

void show_image(vector<int> G, int dim = 28) {


}

ld sigmoid(ld Z) {
    return 1.0/(1.0 + exp(-Z));
}
pair<vector<ld>,vector<ld>> sigmoid(vector<ld> Z) {
    vector<ld> A;
    for(auto u : Z) A.push_back(sigmoid(u));
    return {A,Z};
} 
ld relu(ld Z) {
    return max((ld)0.0,Z);
}
pair<vector<ld>,vector<ld>> relu(vector<ld> Z) {
    vector<ld> A;
    for(auto u : Z) A.push_back(relu(u));
    return {A,Z};
}

vector<ld> sigmoid_backward(vector<ld> dA, vector<ld> Z) {
    vector<ld> dZ, s = sigmoid(Z).first;
    rep(i,0,dA.size()) {
        dZ.push_back(dA[i] * s[i] * ((ld)1-s[i]));
    }
    return dZ;
}
vector<ld> relu_backward(vector<ld> dA, vector<ld> Z) {
    vector<ld> dZ = dA;
    rep(i,0,dA.size()) {
        if (Z[i] < 0) dZ[i] = 0;
    }
    return dZ;
}
// vector<ld> softmax_backward(vector<ld> Z, vector<ld> cache) {
//     Z = cache;
//     ll length = 10;
//     vector<ld> dZ(42000,10)
// }


int main()
{

    fstream fin;
    fin.open("data/mnist_train.csv", ios::in);

    rep(i,0,5) {
        string S; fin >> S;
        auto t = refine(S);
        cout << t.first << endl;
        auto vec = t.second;
        rep(j,0,28) {
            rep(k,0,28) {
                if (vec[j*28+k] == 0) cout << " ";
                else if (vec[j*28+k] < 128) cout << ".";
                else cout << "#";
            }
            cout << endl;
        }
    }


    return 0;
}