#include <Rcpp.h>
#include <iostream>
#include <vector>

#ifdef __APPLE__
#include <sys/types.h>
#include <sys/sysctl.h>
#elif __linux__
#include <fstream>
#include <sstream>
#elif _WIN32
#include <windows.h>
#endif

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

// Function to get operating system name
std::string getOSName() {
#ifdef _WIN32
  return "Windows";
#elif __APPLE__ || __MACH__
  return "macOS";
#elif __linux__
  return "Linux";
#elif __FreeBSD__
  return "FreeBSD";
#else
  return "Other";
#endif
}

// Function to get CPU information
std::string getCPUInfo() {
  std::string cpuInfo = "Unknown";
#ifdef __APPLE__
  char cpuBrand[256];
  size_t size = sizeof(cpuBrand);
  if (sysctlbyname("machdep.cpu.brand_string", &cpuBrand, &size, NULL, 0) == 0) {
    cpuInfo = cpuBrand;
  }
#elif __linux__
  std::ifstream cpuinfo("/proc/cpuinfo");
  std::string line;
  while (std::getline(cpuinfo, line)) {
    if (line.substr(0, 10) == "model name") {
      size_t pos = line.find(":");
      if (pos != std::string::npos) {
        cpuInfo = line.substr(pos + 2);
        break;
      }
    }
  }
#elif _WIN32
  HKEY hKey;
  if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, "HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0", 0, KEY_READ, &hKey) == ERROR_SUCCESS) {
    char value[256];
    DWORD size = sizeof(value);
    if (RegQueryValueEx(hKey, "ProcessorNameString", NULL, NULL, (LPBYTE)&value, &size) == ERROR_SUCCESS) {
      cpuInfo = value;
    }
    RegCloseKey(hKey);
  }
#endif
  return cpuInfo;
}

// Function to get GPU and OpenCL information
Rcpp::List getGPUAndOpenCLInfo() {
  std::vector<std::string> gpuInfo;
  std::vector<std::string> openCLVersion;
  
  cl_uint numPlatforms;
  clGetPlatformIDs(0, nullptr, &numPlatforms);
  
  if (numPlatforms == 0) {
    return Rcpp::List::create(
      Rcpp::Named("GPU") = Rcpp::wrap(gpuInfo),
      Rcpp::Named("OpenCL Version") = Rcpp::wrap(openCLVersion)
    );
  }
  
  std::vector<cl_platform_id> platforms(numPlatforms);
  clGetPlatformIDs(numPlatforms, platforms.data(), nullptr);
  
  for (cl_uint i = 0; i < numPlatforms; ++i) {
    cl_uint numDevices = 1; // Assuming each platform has exactly one GPU device
    
    std::vector<cl_device_id> devices(numDevices);
    clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_GPU, numDevices, devices.data(), nullptr);
    
    for (cl_uint j = 0; j < numDevices; ++j) {
      char deviceName[256];
      clGetDeviceInfo(devices[j], CL_DEVICE_NAME, sizeof(deviceName), deviceName, nullptr);
      
      char version[256];
      clGetPlatformInfo(platforms[i], CL_PLATFORM_VERSION, sizeof(version), version, nullptr);
      
      gpuInfo.push_back(deviceName);
      openCLVersion.push_back(version);
    }
  }
  
  return Rcpp::List::create(
    Rcpp::Named("GPU") = Rcpp::wrap(gpuInfo),
    Rcpp::Named("OpenCL Version") = Rcpp::wrap(openCLVersion)
  );
}

// Function to get system information
// [[Rcpp::export]]
Rcpp::List getSystemInfo() {
  // Gather system information
  std::string osName = getOSName();
  std::string cpuInfo = getCPUInfo();
  Rcpp::List gpuAndOpenCLInfo = getGPUAndOpenCLInfo();
  
  // Return as R list
  return Rcpp::List::create(
    Rcpp::Named("1. Operating System") = osName,
    Rcpp::Named("2. CPU") = cpuInfo,
    Rcpp::Named("3. GPU") = gpuAndOpenCLInfo["GPU"],
                                            Rcpp::Named("4. OpenCL Version") = gpuAndOpenCLInfo["OpenCL Version"]
  );
}

// Expose the function to R
RcppExport SEXP _getSystemInfo() {
  return Rcpp::wrap(getSystemInfo());
}




