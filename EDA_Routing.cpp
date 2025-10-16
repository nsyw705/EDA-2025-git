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

	for (int i = 0; i < numFPGA; i++)
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
			if (index < numFPGA)
			{
				weight_matrix[i][index] = std::stoi(token);
				index++;
			}
		}
		if (index != numFPGA)
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


int main(int argc, char** argv)
{
	if (argc == 7)
	{
		/*命令行参数输入顺序*/
		caseName = argv[1]; //case文件夹名字
		designInfo = argv[2]; // design.info
		designNet = argv[3]; // design.net
		designTopo = argv[4]; //design.topo
		designFpgaOut = argv[5]; //design.fpga.out
		seed = atoi(argv[6]); // 随机数种子
		rep = argv[6];
	}
	else
	{
		cout << "Enter some parameters to run the program: " << endl;
		cout << "<instance file name>  <seed>" << endl;
		exit(0);
	}


	srand(seed);
	read_instance();


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