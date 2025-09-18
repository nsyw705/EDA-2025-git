#include <iostream>
#include <stdlib.h>
#include <fstream>
#include <string.h>
#include <math.h>
#include<algorithm>
#include <string>
#include <cmath> 
#include <ctime>
using namespace std;

int** weight_matrix; //FPGA间的连接通道矩阵
int** delta_weight_matrix; //FPGA间变动的通道数量矩阵

int** nets_count_matrix; //跨越各FPGA之间通道的net数量

int* FPGA_max_weight; //各FPGA最大对外的连接通道数量
int* FPGA_of_nodes; //各逻辑节点对应的FPGA编号

double* net_delay; // 每个net的延时

/*net的结构体*/
typedef struct
{
    int source_node; //net的起点
    int sink_num; // net的终点数量
    int* sink_nodes; //net的终点

    int*** path; //路径，0-1三维矩阵：起点到每个终点选择的边，第一维标记终点序号，后两维度是0-1矩阵表示选择的边
    int** steiner_tree; //所有路径对应的斯坦纳树骨架

    int* path_jump_count; //每条路径跳跨的FPGA数量
    double* path_delay; //每条路径的延时

}Net;


int main()
{
    std::cout << "Hello World!\n";
}

