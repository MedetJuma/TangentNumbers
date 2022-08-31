#include <bits/stdc++.h>

using namespace std;

#define ll long long
#define pii pair < int, int >
#define pb push_back
#define speed ios_base::sync_with_stdio(0), cin.tie(0), cout.tie(0)
#define ull unsigned long long

const int N = 1e6 + 5, INF = 1e9, mod = 1e9 + 7, block = 316;

const ll BIG = 1e18;

const int base = 1000000000;
const int base_digits = 9;

int p = 4;

struct bigint {
    vector<int> a;
    int sign;
 
    bigint() :
        sign(1) {
    }
 
    bigint(long long v) {
        *this = v;
    }
 
    bigint(const string &s) {
        read(s);
    }
 
    void operator=(const bigint &v) {
        sign = v.sign;
        a = v.a;
    }
 
    void operator=(long long v) {
        sign = 1;
        a.clear();
        if (v < 0)
            sign = -1, v = -v;
        for (; v > 0; v = v / base)
            a.push_back(v % base);
    }
 
    bigint operator+(const bigint &v) const             //Addition Operation
    {
        if (sign == v.sign)
        {
            bigint res = v;
 
            for (int i = 0, carry = 0; i < (int) max(a.size(), v.a.size()) || carry; ++i)
            {
                if (i == (int) res.a.size())
                    res.a.push_back(0);
                res.a[i] += carry + (i < (int) a.size() ? a[i] : 0);
                carry = res.a[i] >= base;
                if (carry)
                    res.a[i] -= base;
            }
            return res;
        }
        return *this - (-v);
    }
 
    bigint operator-(const bigint &v) const             //Subtraction Function
    {
        if (sign == v.sign) {
            if (abs() >= v.abs()) {
                bigint res = *this;
                for (int i = 0, carry = 0; i < (int) v.a.size() || carry; ++i)
                {
                    res.a[i] -= carry + (i < (int) v.a.size() ? v.a[i] : 0);
                    carry = res.a[i] < 0;
                    if (carry)
                        res.a[i] += base;
                }
                res.trim();
                return res;
            }
            return -(v - *this);
        }
        return *this + (-v);
    }
 
    void operator*=(int v)                      //Multiplication Function
    {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = 0, carry = 0; i < (int) a.size() || carry; ++i)
        {
            if (i == (int) a.size())
                a.push_back(0);
            long long cur = a[i] * (long long) v + carry;
            carry = (int) (cur / base);
            a[i] = (int) (cur % base);
            //asm("divl %%ecx" : "=a"(carry), "=d"(a[i]) : "A"(cur), "c"(base));
        }
        trim();
    }
 
    bigint operator*(int v) const
    {
        bigint res = *this;
        res *= v;
        return res;
    }
 
    friend pair<bigint, bigint> divmod(const bigint &a1, const bigint &b1)
    {
        int norm = base / (b1.a.back() + 1);
        bigint a = a1.abs() * norm;
        bigint b = b1.abs() * norm;
        bigint q, r;
        q.a.resize(a.a.size());
 
        for (int i = a.a.size() - 1; i >= 0; i--)
        {
            r *= base;
            r += a.a[i];
            int s1 = r.a.size() <= b.a.size() ? 0 : r.a[b.a.size()];
            int s2 = r.a.size() <= b.a.size() - 1 ? 0 : r.a[b.a.size() - 1];
            int d = ((long long) base * s1 + s2) / b.a.back();
            r -= b * d;
            while (r < 0)
                r += b, --d;
            q.a[i] = d;
        }
 
        q.sign = a1.sign * b1.sign;
        r.sign = a1.sign;
        q.trim();
        r.trim();
        return make_pair(q, r / norm);
    }
 
    bigint operator/(const bigint &v) const                 //Division Function
    {
        return divmod(*this, v).first;
    }
 
    bigint operator%(const bigint &v) const                 //Modulus Operation
    {
        return divmod(*this, v).second;
    }
    
    void operator/=(int v)                                  //Shorthand Operation
    {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = (int) a.size() - 1, rem = 0; i >= 0; --i)
        {
            long long cur = a[i] + rem * (long long) base;
            a[i] = (int) (cur / v);
            rem = (int) (cur % v);
        }
        trim();
    }
 
    bigint operator/(int v) const
    {
        bigint res = *this;
        res /= v;
        return res;
    }
 
    int operator%(int v) const
    {
        if (v < 0)
            v = -v;
        int m = 0;
        for (int i = a.size() - 1; i >= 0; --i)
            m = (a[i] + m * (long long) base) % v;
        return m * sign;
    }
 
    void operator+=(const bigint &v)
    {
        *this = *this + v;
    }
    void operator-=(const bigint &v)
    {
        *this = *this - v;
    }
    void operator*=(const bigint &v)
    {
        *this = *this * v;
    }
    void operator/=(const bigint &v)
    {
        *this = *this / v;
    }
 
    bool operator<(const bigint &v) const
    {
        if (sign != v.sign)
            return sign < v.sign;
        if (a.size() != v.a.size())
            return a.size() * sign < v.a.size() * v.sign;
        for (int i = a.size() - 1; i >= 0; i--)
            if (a[i] != v.a[i])
                return a[i] * sign < v.a[i] * sign;
        return false;
    }
 
    bool operator>(const bigint &v) const
    {
        return v < *this;
    }
    bool operator<=(const bigint &v) const
    {
        return !(v < *this);
    }
    bool operator>=(const bigint &v) const
    {
        return !(*this < v);
    }
    bool operator==(const bigint &v) const
    {
        return !(*this < v) && !(v < *this);
    }
    bool operator!=(const bigint &v) const
    {
        return *this < v || v < *this;
    }
 
    void trim()
    {
        while (!a.empty() && !a.back())
            a.pop_back();
        if (a.empty())
            sign = 1;
    }
 
    bool isZero() const
    {
        return a.empty() || (a.size() == 1 && !a[0]);
    }
 
    bigint operator-() const
    {
        bigint res = *this;
        res.sign = -sign;
        return res;
    }
 
    bigint abs() const
    {
        bigint res = *this;
        res.sign *= res.sign;
        return res;
    }
 
    long long longValue() const
    {
        long long res = 0;
        for (int i = a.size() - 1; i >= 0; i--)
            res = res * base + a[i];
        return res * sign;
    }
 
    friend bigint gcd(const bigint &a, const bigint &b)             //GCD Function(Euler Algorithm)
    {
        return b.isZero() ? a : gcd(b, a % b);
    }
    friend bigint lcm(const bigint &a, const bigint &b)             //Simple LCM Operation
    {
        return a / gcd(a, b) * b;
    }
 
    void read(const string &s)                                  //Reading a Big Integer
    {
        sign = 1;
        a.clear();
        int pos = 0;
        while (pos < (int) s.size() && (s[pos] == '-' || s[pos] == '+'))
        {
            if (s[pos] == '-')
                sign = -sign;
            ++pos;
        }
        for (int i = s.size() - 1; i >= pos; i -= base_digits)
        {
            int x = 0;
            for (int j = max(pos, i - base_digits + 1); j <= i; j++)
                x = x * 10 + s[j] - '0';
            a.push_back(x);
        }
        trim();
    }
 
    friend istream& operator>>(istream &stream, bigint &v)
    {
        string s;
        stream >> s;
        v.read(s);
        return stream;
    }
 
    friend ostream& operator<<(ostream &stream, const bigint &v)
    {
        if (v.sign == -1)
            stream << '-';
        stream << (v.a.empty() ? 0 : v.a.back());
        for (int i = (int) v.a.size() - 2; i >= 0; --i)
            stream << setw(base_digits) << setfill('0') << v.a[i];
        return stream;
    }
 
    static vector<int> convert_base(const vector<int> &a, int old_digits, int new_digits)
    {
        vector<long long> p(max(old_digits, new_digits) + 1);
        p[0] = 1;
        for (int i = 1; i < (int) p.size(); i++)
            p[i] = p[i - 1] * 10;
        vector<int> res;
        long long cur = 0;
        int cur_digits = 0;
        for (int i = 0; i < (int) a.size(); i++)
        {
            cur += a[i] * p[cur_digits];
            cur_digits += old_digits;
            while (cur_digits >= new_digits)
            {
                res.push_back(int(cur % p[new_digits]));
                cur /= p[new_digits];
                cur_digits -= new_digits;
            }
        }
        res.push_back((int) cur);
        while (!res.empty() && !res.back())
            res.pop_back();
        return res;
    }
 
    typedef vector<long long> vll;
 
    static vll karatsubaMultiply(const vll &a, const vll &b)        //Multiplication using Karatsuba Algorithm
    {
        int n = a.size();
        vll res(n + n);
        if (n <= 32)
        {
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    res[i + j] += a[i] * b[j];
            return res;
        }
 
        int k = n >> 1;
        vll a1(a.begin(), a.begin() + k);
        vll a2(a.begin() + k, a.end());
        vll b1(b.begin(), b.begin() + k);
        vll b2(b.begin() + k, b.end());
 
        vll a1b1 = karatsubaMultiply(a1, b1);
        vll a2b2 = karatsubaMultiply(a2, b2);
 
        for (int i = 0; i < k; i++)
            a2[i] += a1[i];
        for (int i = 0; i < k; i++)
            b2[i] += b1[i];
 
        vll r = karatsubaMultiply(a2, b2);
        for (int i = 0; i < (int) a1b1.size(); i++)
            r[i] -= a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            r[i] -= a2b2[i];
 
        for (int i = 0; i < (int) r.size(); i++)
            res[i + k] += r[i];
        for (int i = 0; i < (int) a1b1.size(); i++)
            res[i] += a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            res[i + n] += a2b2[i];
        return res;
    }
 
    bigint operator*(const bigint &v) const
    {
        vector<int> a6 = convert_base(this->a, base_digits, 6);
        vector<int> b6 = convert_base(v.a, base_digits, 6);
        vll a(a6.begin(), a6.end());
        vll b(b6.begin(), b6.end());
        while (a.size() < b.size())
            a.push_back(0);
        while (b.size() < a.size())
            b.push_back(0);
        while (a.size() & (a.size() - 1))
            a.push_back(0), b.push_back(0);
        vll c = karatsubaMultiply(a, b);
        bigint res;
        res.sign = sign * v.sign;
        for (int i = 0, carry = 0; i < (int) c.size(); i++)
        {
            long long cur = c[i] + carry;
            res.a.push_back((int) (cur % 1000000));
            carry = (int) (cur / 1000000);
        }
        res.a = convert_base(res.a, 6, base_digits);
        res.trim();
        return res;
    }
};

bigint C(int n, int k){
    bigint res = 1;
    for (int i=n-k+1; i<=n; ++i)
        res *= i;
    for (int i=2; i<=k; ++i)
        res /= i;
    return res;
}

void men(int &a, int b){
    if(b != -1) a = min(a, b);
}

int calc(bigint a){
    int res = 0;
    if(a == 0) return -1;
    while(a % 2 == 0){
        a /= 2;
        res++;
    }
    return res;
}

int pluscarry(int a, int b){
    int carry = 0, ans = 0;
    vector < int > va, vb;
    while(a){
        int cur = a % p;
        va.pb(cur);
        a /= p;
    }
    while(b){
        int cur = b % p;
        vb.pb(cur);
        b /= p;
    }
    int as = (int)va.size();
    int bs = (int)vb.size();
    for(int i = as; i <= 30; i++){
        va.pb(0);
    }
    for(int i = bs; i <= 30; i++){
        vb.pb(0);
    }
//    for(auto e : va){
//        cout << e << " ";
//    }
//    cout << endl;
//    for(auto e : vb){
//        cout << e << " ";
//    }
//    cout << endl;
    for(int i = 0; i < va.size(); i++){
        ans += carry;
        int cur = carry + va[i] + vb[i];
        carry = cur / p;
    }
    ans += carry;
    return ans;
}

int multcarry2(ll a, ll b){
    int carry = 0, ans = 0;
    int num[200];
    for(int i = 0; i <= 60; i++){
        num[i] = 0;
    }
    for(int j = 0; j <= 60; j++){
        int bit = (b >> j) & 1;
        a <<= 1;
        if(!bit) continue;
        for(int i = 0; i <= 60; i++){
            num[i] += (a >> i) & 1;
        }
    }
    for(int i = 0; i <= 60; i++){
        ans += carry;
        num[i] += carry;
        carry = 0;
        if(num[i] > 1){
            carry += num[i] / 2;
            num[i] %= 2;
        }
    }
    ans += carry;
    return ans;
}

int multcarry(int a, int b){
    string num1 = "";
    string num2 = "";
    
    while(a){
        int cur = a % p;
        num1 += char(cur + '0');
        a /= p;
    }
    
    while(b){
        int cur = b % p;
        num2 += char(cur + '0');
        b /= p;
    }
    
    reverse(num1.begin(), num1.end());
    reverse(num2.begin(), num2.end());
    
    int len1 = (int)num1.size();
    int len2 = (int)num2.size();
    int ansss = 0;
    
    // will keep the result number in vector
    // in reverse order
    vector<int> result(len1 + len2, 0);
    
    // Below two indexes are used to find positions
    // in result.
    int i_n1 = 0;
    int i_n2 = 0;
    
    // Go from right to left in num1
    for (int i=len1-1; i>=0; i--)
    {
        int carry = 0;
        int n1 = num1[i] - '0';
        
        // To shift position to left after every
        // multiplication of a digit in num2
        i_n2 = 0;
        
        // Go from right to left in num2
        for (int j=len2-1; j>=0; j--)
        {
            ansss += carry;
            // Take current digit of second number
            int n2 = num2[j] - '0';
            
            // Multiply with current digit of first number
            // and add result to previously stored result
            // at current position.
            int sum = n1*n2 + result[i_n1 + i_n2] + carry;
            
            // Carry for next iteration
            carry = sum/p;
            
            // Store result
            result[i_n1 + i_n2] = sum % p;
            
            i_n2++;
        }
        
        // store carry in next cell
        if (carry > 0){
            result[i_n1 + i_n2] += carry;
            ansss += carry;
        }
        // To shift position to left after every
        // multiplication of a digit in num1.
        i_n1++;
    }
    return ansss;
}

bigint mult(int a, int b){
    bigint res = 1;
    for(int i = a; i <= b; i++){
        res *= i;
    }
    return res;
}

int popcount(int a){
    int ans = 0;
    while(a){
        int cur = a % p;
        ans += cur;
        a /= p;
    }
    return ans;
}

std::vector< vector < int > > intersection(std::vector< vector < int > > &v1,
                                      std::vector< vector < int > > &v2){
    std::vector< vector < int > > v3;

    std::sort(v1.begin(), v1.end());
    std::sort(v2.begin(), v2.end());

    std::set_intersection(v1.begin(),v1.end(),
                          v2.begin(),v2.end(),
                          back_inserter(v3));
    return v3;
}

int m, n, havetobe, b[N], checke, c[N], sym[N];

bigint l[N], a[N];

bool cond = 1;

bigint formula(int i, int q){
    bigint res = mult(2 * (m - i) + 1, 2 * n) * mult(2 * (m - i) + 1, 2 * n) * (i * (q + 1) - m);
    res *= (mult(2 * m + 1, 2 * n) * m - mult(2 * i + 1, 2 * (n - m + i))) * n;
    
    return res;
}

int good[N];

vector < int > vec;

vector < vector < int > > finale, profinale;

int iamcool(){
    int ans = INF;
    sort(b + 1, b + 3 + 1);
//    for(int i = 1; i <= 3; i++){
//        cout << b[i] << " ";
//    }
//    cout << endl;
    /*for(int i = 1; i <= 3; i++){
        cout << popcount(b[i]) << " ";
    }
    cout << endl;
    for(int i = 1; i <= 3; i++){
        for(int j = i + 1; j <= 3; j++){
            cout << popcount(b[i] + b[j]) << " ";
        }
    }
    cout << popcount(b[1] + b[2] + b[3]) << endl;*/
//    for(int i = 1; i <= 3; i++){
//        vec.pb(popcount(b[i]));
//    }
//    for(int i = 1; i <= 3; i++){
//        for(int j = i + 1; j <= 3; j++){
//            vec.pb(popcount(b[i] + b[j]));
//        }
//    }
//    vec.pb(popcount(b[1] + b[2] + b[3]));
    do{
        int res = pluscarry(b[1], b[2]) + pluscarry(b[1] + b[2], b[3]);
        ans = min(ans, res);
        cout << res << endl;
    }while(next_permutation(b + 1, b + 3 + 1));
    cout << endl;
    return ans;
}

void rec(int v, int cur){
    if(v >= vec.size()){
        if(cur == checke){
            vector < int > vectorus;
            for(int i = 0; i < v; i++){
//                if(sym[i] == 1){
//                    cout << "+";
//                }
//                else{
//                    cout << "-";
//                }
//                cout << c[i] << " ";
                if(sym[i] == 2){
                    vectorus.pb(-c[i]);
                }
                else{
                    vectorus.pb(c[i]);
                }
            }
//            cout << endl;
            if(cond){
                finale.pb(vectorus);
            }
            else{
                for(int i = 0; i < finale.size(); i++){
                    if(good[i]) continue;
                    auto e = finale[i];
                    if(e == vectorus){
                        good[i] = 1;
                        profinale.pb(e);
                        break;
                    }
                }
            }
        }
        return;
    }
    for(int i = 0; i <= 10; i++){
        c[v] = i;
        sym[v] = 1;
        rec(v + 1, cur + i * vec[v]);
        if(!i) continue;
        sym[v] = 2;
        rec(v + 1, cur - i * vec[v]);
    }
}

bigint pw(bigint a, int b){
    bigint res = 1;
    for(int i = 1; i <= b; i++){
        res *= a;
    }
    return res;
}

void andre(){
    n = 1e3;
    cout << n << " " << "started" << endl;
    a[1] = 1ll;
    a[0] = 1ll;
    for(int i = 2; i <= n; i++){
        for(int k = 0; k < i; k += 2){
            a[i] += C(i - 1, k) * a[k] * a[i - 1 - k];
        }
        // a[i] /= 2;
        if(a[i] % 7 == 0){
            cout << i << "-th number is divisible by 7" << endl;
        }
        if(a[i] % 11 == 0){
            cout << i << "-th number is divisible by 11" << endl;
        }
        if(a[i] % 19 == 0){
            cout << i << "-th number is divisible by 19" << endl;
        }
        if(a[i] % 23 == 0){
            cout << i << "-th number is divisible by 23" << endl;
        }
    }
}

int main(){
    n = 100;
    p = 2;
    for(int q = 1; q <= 100; q++){
        l[0] = 1;
        for(int i = 1; i <= 2 * n; i++) l[i] = 0;
        for(int cur = 1; cur <= n; cur++){
            for(int i = 1; i <= cur; i++){
                l[2 * cur] += C(2 * cur, 2 * i) * (2 * i * (q + 1) - 2 * cur) * l[2 * (cur - i)];
            }
            l[2 * cur] /= 2 * cur;
        }
        for(int cur = 1; cur <= n; cur++){
            for(int cur1 = 1; cur1 <= cur; cur1++){
                bigint diff = l[cur] - l[cur1];
                int t = calc(2 * (n - m));
                int m1 = popcount(m);
                int k = min(t, 2 * m - m1);
                if(diff % pw(2, k) != 0){
                    cout << "ERROR\n";
                    return 0;
                }
            }
        }
    }
    return 0;
    for(int k = 1; k <= 50; k++){
        for(int r = 0; r < 2; r++){
            int q = 3 * k + r;
            l[0] = 1;
            for(int i = 1; i <= 2 * n; i++) l[i] = 0;
            for(int cur = 1; cur <= n; cur++){
                for(int i = 1; i <= cur; i++){
                    l[2 * cur] += C(2 * cur, 2 * i) * (2 * i * (q + 1) - 2 * cur) * l[2 * (cur - i)];
                }
                l[2 * cur] /= 2 * cur;
            }
            for(int cur = 1; cur * 4 <= n * 2; cur++){
                cout << l[cur * 4] % 9 << " " << l[4] % 9 << endl;
                if(l[cur * 4] % 9 != l[4] % 9){
                    cout << "ERROR\n";
                    return 0;
                }
            }
            cout << endl;
            for(int cur = 1; cur * 4 + 2 <= n * 2; cur++){
                cout << l[cur * 4 + 2] % 9 << " " << l[6] % 9 << endl;
                if(l[cur * 4 + 2] % 9 != l[6] % 9){
                    cout << "ERROR\n";
                    return 0;
                }
            }
            cout << "---------------------------------------------------------------------\n";
        }
    }
    p = 2;
    for(n = 1; n <= 100; n++){
        bigint ans = 0, res = 0;
        for(int i = 0; i <= n; i++){
            ans += C(4 * n + 1, 4 * i + 1);
        }
        for(int i = 1; i <= n; i++){
            res += C(4 * n + 1, 4 * i - 1);
        }
        cout << ans - res << ' ' << pw(4, n) * pw(-1, n) << endl;
    }
    return 0;
//    cond = 1;
//    for(int i = 1; i <= 100; i++){
//        for(int j = i; j <= 100; j++){
//            for(int k = j; k <= 100; k++){
//                b[1] = i;
//                b[2] = j;
//                b[3] = k;
//                sort(b + 1, b + 3 + 1);
//                vec.clear();
//                checke = iamcool();
//                if(checke != popcount(i) + popcount(j) + popcount(k) - popcount(i + j + k)){
//                    cout << -1 << endl;
//                }
//            }
//        }
//    }
//    return 0;
//    bigint ans = -1;
//    for(int q = 1; q <= 100; q++){
//        l[0] = 1;
//        for(int i = 1; i <= 2 * n; i++) l[i] = 0;
//        for(int cur = 1; cur <= n; cur++){
//            for(int i = 1; i <= cur; i++){
//                l[2 * cur] += C(2 * cur, 2 * i) * (2 * i * (q + 1) - 2 * cur) * l[2 * (cur - i)];
//            }
//            l[2 * cur] /= 2 * cur;
//        }
//        a[0] = 1;
//        for(int i = 1; i <= 2 * m; i++) a[i] = 0;
//        for(int cur = 1; cur <= m; cur++){
//            for(int i = 1; i <= cur; i++){
//                a[2 * cur] += C(2 * cur, 2 * i) * (2 * i * (q + 1) - 2 * cur) * a[2 * (cur - i)];
//            }
//            a[2 * cur] /= 2 * cur;
//        }
//        bigint cur = calc(l[2 * n] - a[2 * m]);
//        cout << cur << endl;
//        if(cur != -1){ //if cur == -1 means cur == INF
//            if(ans == -1) ans = cur; //if ans == -1 means ans == INF
//            else ans = min(ans, cur);
//        }
//    }
//    cout << ans << "ans" << endl;
//    return 0;
    for(n = 1; n <= 100; n++){
        bigint ans = 0;
        for(int i = 0; i <= n; i++){
            ans += C(4 * n, 4 * i + 2);
        }
        bigint four = 1;
        for(int i = 1; i <= 2 * n - 1; i++){
            four *= 4;
        }
        bigint two = 1;
        if(n % 2) two = -1;
        for(int i = 1; i <= 2 * n - 1; i++){
            two *= 2;
        }
        cout << ((four - 1) * 6) % 9 << endl;
    }
    return 0;
    for(n = 100; n <= 100; n++){
        cout << n << endl;
        int mn = INF;
        vector < int > vec;
        for(int k = 1; k <= 50; k++){
            int q = 3 * k;
            l[0] = 1;
            for(int i = 1; i <= 2 * n; i++) l[i] = 0;
            for(int cur = 1; cur <= n; cur++){
                for(int i = 1; i <= cur; i++){
                    l[2 * cur] += C(2 * cur, 2 * i) * (2 * i * (q + 1) - 2 * cur) * l[2 * (cur - i)];
                }
                l[2 * cur] /= 2 * cur;
            }
            cout << l[4] % 9 << " " << l[6] % 9 << " " << l[2] % 9 << endl;
        }
        cout << endl;
        cout << "---------------------------------------------------------------------\n";
    }
}
