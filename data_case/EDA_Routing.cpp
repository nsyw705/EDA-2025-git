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

/*net的结构体*/
typedef struct{
	int source_node; //net的起点
	int sink_num; // net的终点数量
	int* sink_nodes; //net的终点

	int*** path; //路径，0-1三维矩阵：起点到每个终点选择的边，第一维标记终点序号，后两维度是0-1矩阵表示选择的边
	int** steiner_tree; //所有路径对应的斯坦纳树骨架

	int* path_jump_count; //每条路径跳跨的FPGA数量
	double* path_delay; //每条路径的延时

}Net;

Net* net_list; // 所有net的数组
double* net_delay; // 每个net的延时


void read_instance(){
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

	if (FIC.fail()){ cout << "1-can not open the file " << designInfoName << endl;exit(0);}
	if (FIC.eof()){ cout << "2-can not open the file " << designInfoName << endl;}
	char str_reading[100];
	numFPGA = 0;
	while (!FIC.eof()){
		FIC >> str_reading;
		numFPGA++;
	}
	numFPGA = numFPGA / 2;
	FIC.close();


	FPGA_max_weight = new int[numFPGA+1];
	FIC.open(designInfoName);
	if (FIC.fail()){ cout << "1-can not open the file " << designInfoName << endl;
		exit(0);
	}
	if (FIC.eof()){ cout << "2-can not open the file " << designInfoName << endl;}

	int FPGA_index = 1;
	for (int x = 0; x < numFPGA * 2; x++){
		FIC >> str_reading;
		if (x % 2 != 0){
			FPGA_max_weight[FPGA_index] = atoi(str_reading);
			FPGA_index++;
		}
	}
	FIC.close();

	std::string net_line;
	numNet = 0;
	FIC.open(designNetName);
	if (FIC.fail()){ cout << "1-can not open the file " << designNetName << endl;
		exit(0);
	}
	if (FIC.eof()){cout << "2-can not open the file " << designNetName << endl;}
	while (std::getline(FIC, net_line))
		numNet++;
	FIC.close();

	net_list = new Net[numNet];
	net_delay = new double[numNet];
	int net_index = 0;
	int max_node_index = MININT;
	FIC.open(designNetName);
	if (FIC.fail()){
		cout << "1-can not open the file " << designNetName << endl;
		exit(0);
	}
	if (FIC.eof()){
		cout << "2-can not open the file " << designNetName << endl;
	}
	while (std::getline(FIC, net_line)){
		std::istringstream iss1(net_line);
		std::istringstream iss2(net_line);
		std::string token;
		int sink_count = 0;
		int token_count = 0;
		while (iss1 >> token){
			if (token[0] == 'g')
				sink_count++;
		}
		if (net_index >= numNet){
			cout << "net index over its total num!";
			exit(-1);
		}
		net_delay[net_index] = (double)MAXINT;
		net_list[net_index].sink_num = sink_count - 1;
		int sink_total = net_list[net_index].sink_num;

		net_list[net_index].sink_nodes = new int[sink_total];
		net_list[net_index].path = new int** [sink_total];
		for (int i = 0; i < sink_total; i++){
			net_list[net_index].sink_nodes[i] = -1;
			net_list[net_index].path[i] = new int* [numFPGA+1];
			for (int x = 0; x < numFPGA+1; x++){
				net_list[net_index].path[i][x] = new int[numFPGA+1];
				for (int y = 0; y < numFPGA+1; y++)
					net_list[net_index].path[i][x][y] = 0;
			}

		}
		net_list[net_index].steiner_tree = new int* [numFPGA+1];
		for (int x = 0; x < numFPGA+1; x++){
			net_list[net_index].steiner_tree[x] = new int[numFPGA+1];
			for (int y = 0; y < numFPGA+1; y++)
				net_list[net_index].steiner_tree[x][y] = 0;
		}
		net_list[net_index].path_jump_count = new int[sink_total];
		net_list[net_index].path_delay = new double[sink_total];

		for (int i = 0; i < sink_total; i++){
			net_list[net_index].path_jump_count[i] = 0;
			net_list[net_index].path_delay[i] = 0;
		}

		sink_count = 0;
		token_count = 0;
		while (iss2 >> token){
			if (token[0] == 'g'){
				if (token_count == 0){
					net_list[net_index].source_node = std::stoi(token.substr(1));
					if (net_list[net_index].source_node>max_node_index){
						max_node_index = net_list[net_index].source_node;
					}
					token_count++;
				}
				else{
					if (sink_count < net_list[net_index].sink_num){
						net_list[net_index].sink_nodes[sink_count] = std::stoi(token.substr(1));
						if (net_list[net_index].sink_nodes[sink_count] > max_node_index)
							max_node_index = net_list[net_index].sink_nodes[sink_count];
						sink_count++;
						token_count++;
					}
					else{
						cout << "net " << net_index << " sink " << sink_count << "over the actual number!";
						exit(-1);
					}

				}
			}

		}
		if (sink_count != net_list[net_index].sink_num)
		{
			cout << "sink node count error! should be " << net_list[net_index].sink_num << " actual " << sink_count;
			exit(-1);
		}
		net_index++;
	}

	numNode = max_node_index;

	FIC.close();


	weight_matrix = new int* [numFPGA+1];
	delta_weight_matrix = new int* [numFPGA+1];
	nets_count_matrix = new int* [numFPGA+1];

	for (int i = 0; i < numFPGA+1; i++){
		weight_matrix[i] = new int[numFPGA+1];
		delta_weight_matrix[i] = new int[numFPGA+1];
		nets_count_matrix[i] = new int[numFPGA+1];
		for (int j = 0; j < numFPGA+1; j++){
			weight_matrix[i][j] = 0;
			delta_weight_matrix[i][j] = 0;
			nets_count_matrix[i][j] = 0;
		}
	}

	std::string topo_line;
	FIC.open(designTopoName);
	if (FIC.fail()){
		cout << "1-can not open the file " << designTopoName << endl;
		exit(0);
	}
	if (FIC.eof()){
		cout << "2-can not open the file " << designTopoName << endl;
	}

	for (int i = 0; i < numFPGA; i++){
		if (i == 0)
			continue;
		FIC >> topo_line;
		FIC >> topo_line;
		std::stringstream ss(topo_line);
		std::string token;
		int index = 1;
		while (std::getline(ss, token, ',')){
			if (index < numFPGA){
				weight_matrix[i][index] = std::stoi(token);
				index++;
			}
		}
		if (index != numFPGA){
			cout << "weight_matrix size error! should be " << numFPGA << " actual " << index;
			exit(-1);
		}
	}
	FIC.close();


	nodes_FPGA = new int[numNode+1]; 
	for (int x = 0; x < numNode + 1; x++)
		nodes_FPGA[x] = 0;

	length_FPGA_nodes = new int[numFPGA+1];
	FPGA_nodes = new int*[numFPGA+1]; 
	for (int i = 0; i < numFPGA+1; i++){
		length_FPGA_nodes[i] = 0;
		FPGA_nodes[i] = new int[numNode+1];
		for (int j = 0; j < numNode+1; j++)
			FPGA_nodes[i][j] = -1;
	}


	std::string fpga_out_line;
	FIC.open(designFpgaOutName);
	if (FIC.fail()){
		cout << "1-can not open the file " << designFpgaOutName << endl;
		exit(0);
	}
	if (FIC.eof()){
		cout << "2-can not open the file " << designFpgaOutName << endl;
	}
	
	int fpga_index = 1;
	while (std::getline(FIC, fpga_out_line)){
		std::istringstream iss(fpga_out_line);
		std::string token;
		int nodes_count = 1;
		while (iss >> token){
			if (token[0] == 'g' && fpga_index < numFPGA+1 && nodes_count< numNode + 1){
				FPGA_nodes[fpga_index][nodes_count] = std::stoi(token.substr(1));
				if(FPGA_nodes[fpga_index][nodes_count] < numNode+1)
					nodes_FPGA[FPGA_nodes[fpga_index][nodes_count]] = fpga_index;
				nodes_count++;
			}
		}
		if(fpga_index < numFPGA + 1)
			length_FPGA_nodes[fpga_index] = nodes_count-1;

		fpga_index++;
	}


	FIC.close();

	cout << "Successfully read and load all the data!" << endl;

}
void generate_solution_dp(){
    
}


int main(int argc, char** argv){
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
	cout << numFPGA<<' '<<numNet<<' '<< numNode<<endl;

	delete[] FPGA_max_weight;

	for (int i = 0; i < numNet; i++){
		for (int x = 0; x < net_list[i].sink_num; x++){
			for (int y = 0; y < numFPGA+1; y++)
				delete[] net_list[i].path[x][y];
			delete[] net_list[i].path[x];
		}
		delete[] net_list[i].path;

		for (int x = 0; x < numFPGA+1; x++)
			delete[] net_list[i].steiner_tree[x];
		delete[] net_list[i].steiner_tree;


		delete[] net_list[i].sink_nodes;
		delete[] net_list[i].path_jump_count;
		delete[] net_list[i].path_delay;
	}

	delete[] net_list;
	delete[] net_delay;


	for (int x = 0; x < numFPGA+1; x++){
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