#include <iostream>
#include <vector>
#include <cassert>

namespace arare{
    template<typename T>
    std::vector<T> range(T start,T next,T end,bool right_close){
        T step = next-start;
        if(start < end and step > 0){
            std::vector<T> ret;
            if(right_close){
                for(T i=start;i<=end;i+=step){
                    ret.push_back(i);
                }
                return ret;
            }else{
                for(T i=start;i<end;i+=step){
                    ret.push_back(i);
                }
                return ret;
            }
        }
        if(start > end and step < 0){
            std::vector<T> ret;
            if(right_close){
                for(T i=start;i>=end;i+=step){
                    ret.push_back(i);
                }
                return ret;
            }else{
                for(T i=start;i>end;i+=step){
                    ret.push_back(i);
                }
                return ret;
            }
        }
        return {};
    }
    template<typename T>
    std::vector<T> range(T start,T end,bool right_close){
        if(start < end){
            return range(start,start+1,end,right_close);
        }else{
            return range(start,start-1,end,right_close);
        }
    }
    struct Unit{
    };
}
template<typename T>
arare::Unit print(T v){
    std::cout << v << std::endl;
    return arare::Unit();
}

