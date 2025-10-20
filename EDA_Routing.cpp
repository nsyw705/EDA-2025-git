/*****************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!注意!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
算例中所有编号都是以1开始，为方便对应，当前数据结构索引为0的位置全部空余，有效索引从1开始
操作时注意：索引全部由1开始！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
*****************************************************************************************/


#include <iostream>
#include <stdlib.h>
#include <fstream>
#include <string.h>
#include <math.h>
#include<algorithm>
#include <string>
#include <cmath> 
#include <ctime>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <utility> 
#include <functional>
#include <iomanip>
#include <numeric>
using namespace std;

#define MININT -2147483648
#define MAXINT 2147483647
#define RANDMAX 999
#define SDF_MAX(a,b) ((a)>(b)?(a):(b))
#define SDF_MIN(a,b) ((a)<(b)?(a):(b))

//***********************************************************//
char* rep;
char nameFinalResult[256];
char nameSchedule[256];
char cutname[256];
//***********************************************************//

char* caseName;
char* designInfo;
char* designNet;
char* designTopo;
char* designFpgaOut;
int seed;

int numFPGA; //FPGA数量
int numNet; //Net数量
int numNode; //逻辑节点数量

int** weight_matrix; //FPGA间的连接通道矩阵
int** delta_weight_matrix; //FPGA间变动的通道数量矩阵

int** nets_count_matrix; //跨越各FPGA之间通道的net数量

int* FPGA_max_weight; //各FPGA最大对外的连接通道数量
int* nodes_FPGA; //各逻辑节点对应的FPGA编号
int* length_FPGA_nodes; //各FPGA对应的逻辑节点数量
int** FPGA_nodes; // 各FPGA对应的逻辑节点列表


struct PairHash
{
	size_t operator()(const std::pair<int, int>& p) const noexcept
	{
		// 常见的组合哈希写法：避免简单拼接带来的冲突
		return std::hash<int>{}(p.first) ^ (std::hash<int>{}(p.second) << 1);
	}
};

/*net的结构体*/
typedef struct
{
	int source_node; //net的起点
	int sink_num; // net的终点数量
	vector<int> sink_nodes; //net的终点
	vector<pair<int, int>> available_edges; // net中节点可用的边（即存在连接线，可使用的边）
	vector<vector<pair<int, int>>> path; //路径，0-1三维矩阵：起点到每个终点选择的边，第一维标记终点序号，后两维度是0-1矩阵表示选择的边
	unordered_set<pair<int, int>, PairHash> steiner_tree; //所有路径对应的斯坦纳树骨架
	vector<int> path_jump_count; //每条路径跳跨的FPGA数量
	vector<double> path_delay; //每条路径的延时

}Net;
vector<Net> net_list; // 所有net的数组
double* net_delay; // 每个net的延时

void read_instance()
{
	char temp_caseName[50];
	char designInfoName[50];
	char designNetName[50];
	char designTopoName[50];
	char designFpgaOutName[50];

	strcpy_s(temp_caseName, caseName);
	strcat_s(temp_caseName, sizeof(temp_caseName), "/");
	strcpy_s(designInfoName, temp_caseName);
	strcpy_s(designNetName, temp_caseName);
	strcpy_s(designTopoName, temp_caseName);
	strcpy_s(designFpgaOutName, temp_caseName);

	strcat_s(designInfoName, sizeof(designInfoName), designInfo);
	strcat_s(designNetName, sizeof(designNetName), designNet);
	strcat_s(designTopoName, sizeof(designTopoName), designTopo);
	strcat_s(designFpgaOutName, sizeof(designFpgaOutName), designFpgaOut);

	ifstream FIC;
	FIC.open(designInfoName);

	if (FIC.fail())
	{
		cout << "1-can not open the file " << designInfoName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designInfoName << endl;
	}
	char str_reading[100];
	numFPGA = 0;
	while (!FIC.eof())
	{
		FIC >> str_reading;
		numFPGA++;
	}
	numFPGA = numFPGA / 2;
	FIC.close();


	FPGA_max_weight = new int[numFPGA + 1];
	FIC.open(designInfoName);
	if (FIC.fail())
	{
		cout << "1-can not open the file " << designInfoName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designInfoName << endl;
	}

	int FPGA_index = 1;
	for (int x = 0; x < numFPGA * 2; x++)
	{
		FIC >> str_reading;
		if (x % 2 != 0)
		{
			FPGA_max_weight[FPGA_index] = atoi(str_reading);
			FPGA_index++;
		}
	}
	FIC.close();

	std::string net_line;
	numNet = 0;
	int net_index = 0;
	int max_node_index = MININT;
	FIC.open(designNetName);
	if (FIC.fail())
	{
		cout << "1-can not open the file " << designNetName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designNetName << endl;
	}

	while (std::getline(FIC, net_line))
	{
		Net read_net;
		std::istringstream iss(net_line);
		std::string token;
		int node_count = 0;
		int token_count = 0;
		int sink_count = 0;
		while (iss >> token)
		{
			if (token[0] == 'g')
			{
				if (token_count == 0)
				{
					read_net.source_node = std::stoi(token.substr(1));
					if (read_net.source_node > max_node_index)
						max_node_index = read_net.source_node;
					token_count++;
				}
				else
				{
					read_net.sink_nodes.push_back(std::stoi(token.substr(1)));
					if (std::stoi(token.substr(1)) > max_node_index)
						max_node_index = std::stoi(token.substr(1));
					sink_count++;
					token_count++;
				}
			}

		}
		read_net.sink_num = (int)read_net.sink_nodes.size();
		net_list.push_back(read_net);
		numNet++;
	}

	net_delay = new double[numNet];
	for (int x = 0; x < numNet; x++)
		net_delay[x] = (double)MAXINT;
	numNode = max_node_index;
	FIC.close();

	weight_matrix = new int* [numFPGA + 1];
	delta_weight_matrix = new int* [numFPGA + 1];
	nets_count_matrix = new int* [numFPGA + 1];

	for (int i = 0; i < numFPGA + 1; i++)
	{
		weight_matrix[i] = new int[numFPGA + 1];
		delta_weight_matrix[i] = new int[numFPGA + 1];
		nets_count_matrix[i] = new int[numFPGA + 1];
		for (int j = 0; j < numFPGA + 1; j++)
		{
			weight_matrix[i][j] = 0;
			delta_weight_matrix[i][j] = 0;
			nets_count_matrix[i][j] = 0;
		}
	}

	std::string topo_line;
	FIC.open(designTopoName);
	if (FIC.fail())
	{
		cout << "1-can not open the file " << designTopoName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designTopoName << endl;
	}

	for (int i = 0; i <= numFPGA; i++)
	{
		if (i == 0)
			continue;
		FIC >> topo_line;
		FIC >> topo_line;
		std::stringstream ss(topo_line);
		std::string token;
		int index = 1;
		while (std::getline(ss, token, ','))
		{
			if (index <= numFPGA)
			{
				weight_matrix[i][index] = std::stoi(token);
				index++;
			}
		}
		if (index != numFPGA+1)
		{
			cout << "weight_matrix size error! should be " << numFPGA << " actual " << index;
			exit(-1);
		}
	}
	FIC.close();


	nodes_FPGA = new int[numNode + 1];
	for (int x = 0; x < numNode + 1; x++)
		nodes_FPGA[x] = 0;

	length_FPGA_nodes = new int[numFPGA + 1];
	FPGA_nodes = new int* [numFPGA + 1];
	for (int i = 0; i < numFPGA + 1; i++)
	{
		length_FPGA_nodes[i] = 0;
		FPGA_nodes[i] = new int[numNode + 1];
		for (int j = 0; j < numNode + 1; j++)
			FPGA_nodes[i][j] = -1;
	}


	std::string fpga_out_line;
	FIC.open(designFpgaOutName);
	if (FIC.fail())
	{
		cout << "1-can not open the file " << designFpgaOutName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designFpgaOutName << endl;
	}

	int fpga_index = 1;
	while (std::getline(FIC, fpga_out_line))
	{
		std::istringstream iss(fpga_out_line);
		std::string token;
		int nodes_count = 1;
		while (iss >> token)
		{
			if (token[0] == 'g' && fpga_index < numFPGA + 1 && nodes_count < numNode + 1)
			{
				FPGA_nodes[fpga_index][nodes_count] = std::stoi(token.substr(1));
				if (FPGA_nodes[fpga_index][nodes_count] < numNode + 1)
					nodes_FPGA[FPGA_nodes[fpga_index][nodes_count]] = fpga_index;
				nodes_count++;
			}
		}
		if (fpga_index < numFPGA + 1)
			length_FPGA_nodes[fpga_index] = nodes_count - 1;

		fpga_index++;
	}


	FIC.close();


	for (int x = 0; x < net_list.size(); x++)
	{
		for (int y = 0; y < net_list[x].sink_nodes.size(); y++)
		{
			int soure_FPGA = nodes_FPGA[net_list[x].source_node];
			int sink_FPGA = nodes_FPGA[net_list[x].sink_nodes[y]];
			if (weight_matrix[soure_FPGA][sink_FPGA] > 0)
			{
				net_list[x].available_edges.push_back(make_pair(soure_FPGA, sink_FPGA));
				net_list[x].available_edges.push_back(make_pair(sink_FPGA, soure_FPGA));
			}
		}
	}

	cout << "Successfully read and load all the data!" << endl;

}
void check_read_instance(){
	cout << numFPGA << ' ' << numNet << ' ' << numNode << endl;
	cout << "FPGA_max_weight: ";for (int i = 1; i <= numFPGA; i++){cout << FPGA_max_weight[i] << ' ';}cout << endl;
	cout << "weight_matrix: " << endl;
	for (int i = 1; i <= numFPGA; i++){for (int j = 1; j <= numFPGA; j++){
			cout << weight_matrix[i][j] << ' ';
		}
		cout << endl;
	}
	cout << "delta_weight_matrix:" << endl;
	for (int i = 1; i <= numFPGA; i++)
	{
		for (int j = 1; j <= numFPGA; j++)
		{
			cout << delta_weight_matrix[i][j] << ' ';
		}
		cout << endl;
	}
	cout << "nets_count_matrix: " << endl;
	for (int i = 1; i <= numFPGA; i++)
	{
		for (int j = 1; j <= numFPGA; j++)
		{
			cout << nets_count_matrix[i][j] << ' ';
		}
		cout << endl;
	}
	cout << "nodes_FPGA: ";
	for (int i = 1; i <= numNode; i++)
	{
		cout << nodes_FPGA[i] << ' ';
	}
	cout << endl;
	cout << "length_FPGA_nodes: ";
	for (int i = 1; i <= numFPGA; i++)
	{
		cout << length_FPGA_nodes[i] << ' ';
	}
	cout << endl;
	cout << "FPGA_nodes: " << endl;
	for (int i = 1; i <= numFPGA; i++)
	{
		for (int j = 1; j <= length_FPGA_nodes[i]; j++)
		{
			cout << FPGA_nodes[i][j] << ' ';
		}
		cout << endl;
	}
}
int ceil8(double x){return static_cast<int>(std::ceil(x / 8.0) * 8.0);} // 取8的倍数
vector<vector<float>> current_cost(int **weight_matrix,  int **nets_count_matrix) // 当前的成本，也就是选择每条边的成本
{ 
	vector<vector<float>> costn(numFPGA,vector<float>(numFPGA,MAXINT));
	for(int i=0;i<numFPGA;i++){costn[i][i]=0;}
	for(int i=1;i<=numFPGA;i++){//计算下一次使用的成本 
		 for(int j=i;j<=numFPGA;j++){
			if(j==i||weight_matrix[i][j]==0){continue;}
			costn[i-1][j-1]=30+0.7*ceil8((float)(nets_count_matrix[i][j]+1)/weight_matrix[i][j]);
			costn[j-1][i-1]=costn[i-1][j-1];
		 }
	}
	return costn;
}
void multi_dijkstra(int source_node, vector<int> sink_node, vector<vector<pair<int, int>>>& path){
	int k=(int)sink_node.size();
	int n=numFPGA;
	path.assign(k,{});//初始化
	int source_fpga=nodes_FPGA[source_node];//源点，终点对应的FPGA
	vector<int> sinks_fpga(k);
	for (int i = 0; i < k; ++i){sinks_fpga[i] = nodes_FPGA[sink_node[i]];}
	unordered_set<int> fpga_s(sinks_fpga.begin(),sinks_fpga.end());//用于判断起点和终点是否全部搜索到；
	fpga_s.insert(k);
	vector<char> same_fpga(k, 0);//统计源点和终点在同一块fpga的情况
	int nums_same=0;
	for (int i = 0; i < k; ++i){
		if (sinks_fpga[i] == source_fpga){same_fpga[i] = 1;nums_same++;
		} // 路径为空，不占跨 FPGA 边
		if(nums_same==k){return;}//源点和终点都在同一块fpga,直接return；
	}
	// —— Dijkstra
	const float INF = MAXINT;
	vector<float> dist(n, INF);//都是0开始，后续使用的时候 注意索引
	vector<int> parent(n, -1);
	vector<char> vis(n, 0);
	dist[source_fpga-1] = 0;
	vector<vector<float>>gCost=current_cost(weight_matrix,nets_count_matrix);
	for (int it = 0; it < n; it++){
		int u = -1;
		float best = INF;
		for (int i = 0; i < n; i++){if (!vis[i] && dist[i] < best){best = dist[i];u = i;}}
		if (u == -1){break;}
		vis[u] = 1;fpga_s.erase(u+1);
		// if(fpga_s.empty()){break;}//检查起点和终点是否都搜索完
		for (int v = 0; v < n; ++v){
			float w = gCost[u][v];
			if (w >= INF){continue;}
			if (dist[v] > dist[u] + w){
				dist[v] = dist[u] + w;
				parent[v] = u;
			}
		}
	}
	// 逐个终点回溯路径
	for (int i = 0; i < k; ++i){
		if (same_fpga[i]){path[i].clear(); continue;}
		int t = sinks_fpga[i]-1;
		if (dist[t] >= INF){
			path[i].clear(); // 不可达
			continue;
		}
		vector<pair<int, int>> edges_rev;
		int v = t;
		while (v != source_fpga-1)
		{
			int u = parent[v];
			if (u < 0){
				edges_rev.clear();
				break;
			} 
			edges_rev.push_back({u+1, v+1});//索引从0开始，这里调整为1；
			v = u;
		}
		reverse(edges_rev.begin(), edges_rev.end());
		path[i] = std::move(edges_rev);
	}
}   
void ge_su_dp(){//初始化
	for(int n=0;n<numNet;n++){
		auto& net=net_list[n];
		multi_dijkstra(net.source_node, net.sink_nodes, net.path);
		// 更新累计使用次数
		for (auto &edges : net.path){
			for (auto &e : edges){
				int u = e.first, v = e.second;
				nets_count_matrix[u][v] += 1;
				nets_count_matrix[v][u] += 1;
			}
		}
	}
}
void calculate_su(){//计算net的delay
	for(int i=0;i<numNet;i++){
			net_delay[i]=0;//net的整体delay初始化为0；
			int sink_num=net_list[i].sink_num;
			net_list[i].path_delay.assign(sink_num,0);//分配空间，同时初始化0
			net_list[i].path_jump_count.assign(sink_num,0);
			auto& path_jump=net_list[i].path_jump_count;
			auto & path_delay=net_list[i].path_delay;
			auto & path=net_list[i].path;
			for(int j=0;j<sink_num;j++){
				if(path[j].size()==0){continue;}
				for(auto edge:path[j]){
					if(edge.first!=edge.second){path_delay[j] += 30 + 0.7 * ceil8((float)nets_count_matrix[edge.first][edge.second]/ weight_matrix[edge.first][edge.second]);path_jump[j]++;}
				}
				if (path_delay[j] > net_delay[i]){ net_delay[i] = path_delay[j];}
			}
	}
}
void file()//输出design.route.out
{
	std::ofstream out("design.route.out", std::ios::out);
	if (!out.is_open())
		return;
	vector<int> order(numNet);
	iota(order.begin(), order.end(), 0);
	stable_sort(order.begin(), order.end(),
					 [](int a, int b)
					 { return net_delay[a] > net_delay[b]; });

	out.setf(std::ios::fixed);
	out << std::setprecision(1);

	for (int idx : order)
	{
		if (net_delay[idx] <= 0.0)
			continue;

		auto &paths = net_list[idx].path;
		auto &pdelay = net_list[idx].path_delay;

		bool printed_header = false;

		for (int j = 0; j < paths.size() && j < pdelay.size(); j++)
		{
			if (pdelay[j] <= 0.0)
				continue;
			auto &edges = paths[j];
			if (edges.empty())
				continue;

			if (!printed_header)
			{
				out << "[net " << (idx + 1) << "]\n";
				printed_header = true;
			}

			out << '[';
			out << edges.front().first;
			for (int e = 0; e < edges.size(); ++e)
			{
				out << ',' << edges[e].second;
			}
			out << "] [" << pdelay[j] << "]\n";
		}
	}
	out.close();
}

int main(int argc, char** argv)
{
	if (argc == 7){
		/*命令行参数输入顺序*/
		caseName = argv[1]; //case文件夹名字
		designInfo = argv[2]; // design.info
		designNet = argv[3]; // design.net
		designTopo = argv[4]; //design.topo
		designFpgaOut = argv[5]; //design.fpga.out
		seed = atoi(argv[6]); // 随机数种子
		rep = argv[6];
	}
	else{
		cout << "Enter some parameters to run the program: " << endl;
		cout << "<instance file name>  <seed>" << endl;
		exit(0);
	}

	srand(seed);
	read_instance();
	ge_su_dp();
	calculate_su();
	file();
	cout << endl;

	cout<<"search is done!"<<endl;

	delete[] FPGA_max_weight;

	delete[] net_delay;


	for (int x = 0; x < numFPGA + 1; x++)
	{
		delete[] weight_matrix[x];
		delete[] delta_weight_matrix[x];
		delete[] nets_count_matrix[x];
		delete[] FPGA_nodes[x];
	}
	delete[] weight_matrix;
	delete[] delta_weight_matrix;
	delete[] nets_count_matrix;

	delete[] FPGA_nodes;
	delete[] nodes_FPGA;
	delete[] length_FPGA_nodes;
}