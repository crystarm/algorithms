// https://acmp.ru/index.asp?main=task&id_task=990
// DP + sorting + greedy + prefix sums + state reconstruction

#include <bits/stdc++.h>
using namespace std;

typedef long long ll;
typedef vector<int> vi;
typedef vector<ll> vll;

#define all(x) (x).begin(), (x).end()
#define sz(x) (int)((x).size())
#define rep(i,a,b) for (int i = (a); i < (b); i++)
#define pb push_back

const ll LINF = (ll)4e18;

struct kid { ll g; int id; };

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);

    int n, m;
    cin >> n >> m;

    vector<kid> a(n);
    rep(i,0,n)
    {
        cin >> a[i].g;
        a[i].id = i;
    }

    sort(all(a), [](const kid &x, const kid &y) { return x.g > y.g; });

    vll pref(n + 1, 0);
    rep(i,0,n) pref[i + 1] = pref[i] + a[i].g;

    vector<vll> dp(n, vll(m + 1, LINF));
    vector<vi> pp(n, vi(m + 1, -1));
    vector<vi> ps(n, vi(m + 1, -1));

    dp[0][0] = 0;

    rep(q,1,n)
    {
        rep(p,0,q)
        {
            ll add = (ll)p * (pref[q] - pref[p]);

            rep(r,0,q)
            {
                ll best = LINF;
                int bs = -1;

                for (int ns = r; ns <= m; ns += q)
                {
                    int os = ns - q;

                    if (os >= 0 && dp[p][os] < best)
                    {
                        best = dp[p][os];
                        bs = os;
                    }

                    if (best == LINF) continue;

                    ll cur = best + add;
                    if (cur < dp[q][ns])
                    {
                        dp[q][ns] = cur;
                        pp[q][ns] = p;
                        ps[q][ns] = bs;
                    }
                }
            }
        }
    }

    ll best = LINF;
    int lp = 0;
    int bs = 0;

    rep(p,0,n)
    {
        rep(s,0,m + 1)
        {
            if (dp[p][s] == LINF) continue;

            int rem = m - s;
            if (rem < n || rem % n != 0) continue;

            ll cur = dp[p][s] + (ll)p * (pref[n] - pref[p]);
            if (cur < best)
            {
                best = cur;
                lp = p;
                bs = s;
            }
        }
    }

    vi d(n + 1, 0);

    int p = lp;
    int s = bs;

    while (p > 0)
    {
        int op = pp[p][s];
        int os = ps[p][s];

        d[p] = (s - os) / p;

        p = op;
        s = os;
    }

    int z = (m - bs) / n;

    vi res(n, z);

    rep(i,1,n)
    {
        if (!d[i]) continue;
        rep(j,0,i) res[j] += d[i];
    }

    vi ans(n);

    rep(i,0,n) ans[a[i].id] = res[i];

    cout << best << '\n';

    rep(i,0,n)
    { if (i) cout << ' '; cout << ans[i]; }

    cout << '\n';

    return 0;
}
