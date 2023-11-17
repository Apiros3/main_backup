#include <bits/stdc++.h>
using namespace std;

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

int main()
{

    ll N; cin >> N;
    vector<ll> vec(N), A(N+1,INF);
    A[0] = -INF;

    rep(i,0,N) cin >> vec[i];
    rep(i,0,N) {
        ll tmp = lower_bound(A.begin(),A.end(),vec[i]) - A.begin();
        A[tmp] = min(A[tmp],vec[i]);
    }
    rep(i,0,N+1) cout << A[i] << endl;




    return 0;
}