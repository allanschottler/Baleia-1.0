// Tester.cpp
#include <HsFFI.h>
#include "Start_stub.h"
#include <stdio.h>
#include <string>
#include <vector>
#include <iostream>
#include <chrono>
#include <algorithm>

HsStablePtr ctx;

extern "C" {
    void HsStart();
    void HsEnd();
}

auto makeInputs(int argc, char *argv[])
{
    const std::size_t nsamples = std::stoi(std::string{argv[1]});
	std::vector<float> ils(nsamples, 1);
    std::vector<float> irs(nsamples, 1);

    int i = 0;
    std::transform(ils.begin(), ils.end(), ils.begin(), 
        [&](auto v) mutable
        { return v + i++; });
    i = 0;
    std::transform(irs.begin(), irs.end(), irs.begin(), 
        [&](auto v) mutable
        { return v + i++; });    

    return std::make_pair(ils, irs);
}

auto makeInParams(int argc, char *argv[])
{
    std::vector<float> inparams;

    if(argc <= 2)   
        inparams = std::vector<float>(7, 1);
    else {
        std::vector<char*> fs(&argv[2], &argv[argc]);
        std::cout << fs.size() << std::endl;
        std::transform(fs.begin(), fs.end(), 
            std::back_inserter(inparams), 
            [argv](char* f) { 
                std::string str{f};
                std::cout << str << std::endl;
                return std::stof(str); 
            });
    }

    return inparams;
}

int main(int argc, char *argv[])
{	
    std::cout << "Runnning test...\n";    
    
    auto [ils, irs] = makeInputs(argc, argv);
    auto inparams = makeInParams(argc, argv);

    const std::size_t nsamples = std::stoi(std::string{argv[1]});
    std::vector<float> ols, ors, outparams;
    ols.resize(nsamples);
    ors.resize(nsamples);
    outparams.resize(2);
    
    HsStart();
    
    auto controlMeta = std::string(static_cast<char*>(controlInMeta()));
    std::cout << controlMeta << std::endl;

    ctx = startCtx();

    int total = 0;

try{
    for(int j(0); j < 3; ++j)
    {
        //std::cout << "J " << j << std::endl;
        
        // std::cout << "main" << j << std::endl;
        auto start = std::chrono::high_resolution_clock::now();
        //main2(ctx, nsamples, inparams.size(), inparams.data(), outparams.data(), ils.data(), irs.data(), ols.data(), ors.data());
        ctx = main2(ctx, nsamples, inparams.size(), inparams.data(), outparams.data(), ils.data(), irs.data(), ols.data(), ors.data());
        auto stop = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
            
        for(int i(0); i < nsamples; ++i)
            std::cout << "(" << ols[i] << ", " << ors[i] << ")" << std::endl;
        
        // std::cout << "VuPPM: (" << outparams[0] << ", " << outparams[1] << ")" << std::endl;
        // std::cout << "Duration: " << duration.count() << std::endl;
        total += duration.count();
        
        auto [ils, irs] = makeInputs(argc, argv);
    }
} catch(std::exception& e) 
{ std::cout << e.what(); }
    
    std::cout << "Total (us): " << total << std::endl;
    freeCtx(ctx);
    HsEnd();
    return 0;
}
  