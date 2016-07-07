#include <iostream>
#include <vector>
#include <iterator>
#include <stdint.h>

/*
 * Count inversion, as well as sort the input array at once.
 * @param v int[] input array
 * @return number of inversions
 */
uint64_t countInvAndSort(std::vector<int>& v) {
    // Divide algorithm:
    //   Merge solution is simply adding three types of inversion together.
    if (v.size() <= 1) {
        return 0;
    }

    std::vector<int>::iterator mid = v.begin();
    std::advance(mid, v.size() / 2);
    std::vector<int> lf(v.begin(), mid);
    std::vector<int> rg(mid, v.end());

    uint64_t lfCount = countInvAndSort(lf);
    uint64_t riCount = countInvAndSort(rg);
    // Inversion that go across two array:
    uint64_t splitCount = 0;

    int i = 0, j = 0, t = 0, k;
    while(t < v.size()) {
        if (i >= lf.size()) {
            // Copy the things left
            for(k = j; k < rg.size(); k ++) {
                v[t + k - j] = rg[k];
            }
            break;
        } else if (j >= rg.size()) {
            // Copy the things left
            for(k = i; k < lf.size(); k ++) {
                v[t + k - i] = lf[k];
            }
            break;
        }
        if (lf[i] <= rg[j]) {
            v[t] = lf[i];
            i++;
        } else {
            v[t] = rg[j];
            j++;
            // This number form an inversion with every number after it in lf.
            splitCount += (lf.size() - i);
        }
        t++;
    }

    return lfCount + riCount + splitCount;
}

int main( int argc, char **argv )
{
    std::vector<int> v;
    int nCount = 0;
    int n;
    while (!std::cin.eof()) {
        std::cin >> n;
        if (std::cin.fail()) {
            break;
        }
        v.push_back(n);
        nCount ++;
    }
    if (nCount <= 0) {
        std::cerr << "No number!" << std::endl;
        return 1;
    }
    uint64_t count = countInvAndSort(v);
    std::cout << count << std::endl;
    return 0;
}
