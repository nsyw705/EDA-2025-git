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

int** weight_matrix; //FPGA�������ͨ������
int** delta_weight_matrix; //FPGA��䶯��ͨ����������

int** nets_count_matrix; //��Խ��FPGA֮��ͨ����net����

int* FPGA_max_weight; //��FPGA�����������ͨ������
int* FPGA_of_nodes; //���߼��ڵ��Ӧ��FPGA���

double* net_delay; // ÿ��net����ʱ

/*net�Ľṹ��*/
typedef struct
{
    int source_node; //net�����
    int sink_num; // net���յ�����
    int* sink_nodes; //net���յ�

    int*** path; //·����0-1��ά������㵽ÿ���յ�ѡ��ıߣ���һά����յ���ţ�����ά����0-1�����ʾѡ��ı�
    int** steiner_tree; //����·����Ӧ��˹̹�����Ǽ�

    int* path_jump_count; //ÿ��·�������FPGA����
    double* path_delay; //ÿ��·������ʱ

}Net;


int main()
{
    std::cout << "Hello World!\n";
}

