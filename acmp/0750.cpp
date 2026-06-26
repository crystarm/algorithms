// bipartite matching + alternating paths + max matching vertex coverability

#include <bits/stdc++.h>
using namespace std;

typedef vector<int> vi;
typedef string str;
typedef pair<int, int> pii;

#define all(x) (x).begin(), (x).end()
#define sz(x) (int)(x).size()
#define rep(i,a,b) for (int i = (a); i < (b); ++i)
#define rsr(v,n) (v).reserve(n)
#define pb push_back
#define fi first
#define se second

struct hk
{
    int n, m;
    vector<vi> g;
    vi ml, mr, d;

    void init(int n0, int m0)
    {
        n = n0;
        m = m0;
        g.assign(n, vi());
        ml.assign(n, -1);
        mr.assign(m, -1);
        d.resize(n);
    }

    bool bfs()
    {
        queue<int> q;

        rep(i, 0, n)
        {
            if (ml[i] == -1)
            {
                d[i] = 0;
                q.push(i);
            }
            else d[i] = -1;
        }

        bool ok = 0;

        while (!q.empty())
        {
            int v = q.front();
            q.pop();

            for (int to : g[v])
            {
                int u = mr[to];

                if (u == -1) ok = 1;
                else if (d[u] == -1)
                {
                    d[u] = d[v] + 1;
                    q.push(u);
                }
            }
        }

        return ok;
    }

    bool dfs(int v)
    {
        for (int to : g[v])
        {
            int u = mr[to];

            if (u == -1 || (d[u] == d[v] + 1 && dfs(u)))
            {
                ml[v] = to;
                mr[to] = v;
                return 1;
            }
        }

        d[v] = -1;
        return 0;
    }

    int solve()
    {
        int res = 0;

        while (bfs())
        {
            rep(i, 0, n)
            {
                if (ml[i] == -1 && dfs(i)) ++res;
            }
        }

        return res;
    }
};

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n1, n2, m;
    cin >> n1 >> n2 >> m;

    hk H;
    H.init(n1, n2);

    vector<vi> rg(n2);

    rep(i, 0, m)
    {
        int a, b;
        cin >> a >> b;
        --a;
        --b;

        H.g[a].pb(b);
        rg[b].pb(a);
    }

    H.solve();

    str ans1(n1, 'N');
    str ans2(n2, 'N');

    vector<char> vl(n1, 0), vr(n2, 0);
    queue<pii> q;

    rep(i, 0, n1)
    {
        if (H.ml[i] == -1)
        {
            vl[i] = 1;
            q.push({0, i});
        }
    }

    while (!q.empty())
    {
        pii p = q.front();
        q.pop();

        int side = p.fi;
        int v = p.se;

        if (side == 0)
        {
            for (int to : H.g[v])
            {
                if (H.ml[v] == to) continue;

                if (!vr[to])
                {
                    vr[to] = 1;
                    q.push({1, to});
                }
            }
        }
        else
        {
            int to = H.mr[v];

            if (to != -1 && !vl[to])
            {
                vl[to] = 1;
                q.push({0, to});
            }
        }
    }

    rep(i, 0, n1)
    {
        if (vl[i]) ans1[i] = 'P';
    }

    vl.assign(n1, 0);
    vr.assign(n2, 0);

    rep(i, 0, n2)
    {
        if (H.mr[i] == -1)
        {
            vr[i] = 1;
            q.push({1, i});
        }
    }

    while (!q.empty())
    {
        pii p = q.front();
        q.pop();

        int side = p.fi;
        int v = p.se;

        if (side == 1)
        {
            for (int to : rg[v])
            {
                if (H.mr[v] == to) continue;

                if (!vl[to])
                {
                    vl[to] = 1;
                    q.push({0, to});
                }
            }
        }
        else
        {
            int to = H.ml[v];

            if (to != -1 && !vr[to])
            {
                vr[to] = 1;
                q.push({1, to});
            }
        }
    }

    rep(i, 0, n2)
    {
        if (vr[i]) ans2[i] = 'P';
    }

    cout << ans1 << '\n';
    cout << ans2 << '\n';

    return 0;
}
