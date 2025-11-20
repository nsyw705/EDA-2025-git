#include <vector>
#include <queue>
#include <algorithm>
#include <utility>
#include <cassert>
#include<iostream>
using namespace std;

// ---------- 1) 数组版本：保留降序前 k 大 ----------
void topk_desc_array(double *s, int *address, int n, int k)
{
    if (!s || !address || n <= 0 || k <= 0)
        return;
    if (k > n)
        k = n;

    using Node = pair<double, int>; // {值, 地址}
    // 小根堆（堆顶是当前 top-k 中最小的那个）
    auto cmp = [](const Node &a, const Node &b)
    { return a.first > b.first; };
    priority_queue<Node, vector<Node>, decltype(cmp)> heap(cmp);

    for (int i = 0; i < n; ++i)
    {
        if ((int)heap.size() < k)
        {
            heap.emplace(s[i], address[i]);
        }
        else if (s[i] > heap.top().first)
        {
            heap.pop();
            heap.emplace(s[i], address[i]);
        }
    }

    // 取出并按降序排好，得到与“全量降序后取前k”的同序结果
    vector<Node> topk;
    topk.reserve(k);
    while (!heap.empty())
    {
        topk.push_back(heap.top());
        heap.pop();
    }
    sort(topk.begin(), topk.end(), [](const Node &a, const Node &b)
         {
             return a.first > b.first; // 降序
         });

    for (int i = 0; i < k; ++i)
    {
        s[i] = topk[i].first;
        address[i] = topk[i].second;
    }
}

// ---------- 2) 向量版本：保留升序前 k 小 ----------
void topk_asc_vector(vector<double> &s, vector<int> &address, int k)
{
    int n = (int)s.size();
    assert((int)address.size() == n);
    if (n <= 0 || k <= 0){return;}
    if (k > n){k = n;}
    struct Node
    {
        double v;
        int id;
        // 默认优先队列是大根堆；为了“保留最小的 k 个”，我们让堆顶是当前 top-k 中最大的
        bool operator<(const Node &o) const { return v < o.v; } // v 大的更“优先”
    };
    priority_queue<Node> heap; // 大根堆（堆顶是最大）
    for (int i = 0; i < n; ++i)
    {
        if ((int)heap.size() < k)
        {
            heap.push(Node{s[i], address[i]});
        }
        else if (s[i] < heap.top().v)
        {
            heap.pop();
            heap.push(Node{s[i], address[i]});
        }
    }
    vector<Node> topk;
    topk.reserve(k);
    while (!heap.empty())
    {
        topk.push_back(heap.top());
        heap.pop();
    }
    sort(topk.begin(), topk.end(), [](const Node &a, const Node &b)
         {
             return a.v < b.v; // 升序
         });

    for (int i = 0; i < k; ++i)
    {
        s[i] = topk[i].v;
        address[i] = topk[i].id;
    }
}
int main(){
    vector<double> s={1,2,4,3,0};
    vector<int> address={0,1,2,3,4};
    int k=2;
    topk_asc_vector(s,address,k);
    
    // for(auto x:address){
    //     cout<<x<<' ';
    // }
    vector<double> topk_check_delay;
    // topk_check_delay.resize(2);
    topk_check_delay.push_back(1.2);
    topk_check_delay.push_back(1.2);
    topk_check_delay.resize(1);
    cout<<topk_check_delay.size()<<' ';
    cout<<topk_check_delay[0]<<endl;

    return 0;

}
