
This clrng R package is built as a tool set for parallel random number generation on GPUs. 
It is dependent on the gpuR package and utilize the MRG31k3p RNG from the clRNG, an OpenCL library by [Pierre L’Ecuyer](https://www.iro.umontreal.ca/~lecuyer/).


# Installing drivers



The `clrng` and `gpuR` packages require GPU drivers and opencl.  This can be tricky.  Although the package is only officially supported on unix, it should work on any computer where openCL is available.   


Check you have a GPU

```
lspci -kv
```

Below are some instructions for installing the required drivers for Ubuntu.


## Nvidia

See the [Nvidia installation guide](https://docs.nvidia.com/cuda/cuda-installation-guide-linux/index.html#network-repo-installation-for-ubuntu)


If you're lucky, the following will work.


```
sudo apt install nvidia-driver-535
sudo reboot
sudo apt-get install cuda
```

Check you've installed the drivers correctly.

```
nvidia-smi
```

Install openCL

```
sudo apt install -y nvidia-opencl-dev clinfo
```

check openCL is working

```
clinfo
```


## AMD

See the 
[AMD rocm install guide](https://rocm.docs.amd.com/projects/install-on-linux/en/latest/tutorial/quick-start.html)


```
sudo apt install amdgpu-dkms
sudo apt install rocm
sudo reboot
```

Check the drivers

```
/opt/rocm/bin/rocm-smi
```

... and openCL

```
/opt/rocm/bin/clinfo
```


## Amazon AWS

Start an instance with

- `g3s.xlarge`
- ubuntu server 22.04
- create security group, allow ssh traffic from anywhere, https from anywhere
- 20gb storage

Make sure the software is up to date

```
sudo apt update
sudo apt dist-upgrade
sudo reboot
```

Install a version of the kernel with the DRM module

```
sudo apt install -y linux-headers-virtual linux-source linux-image-extra-virtual
sudo apt autoremove
sudo apt clean
sudo reboot
```

Set up the nvidia software repository

```
wget -O /tmp/cuda-keyring.deb https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2204/x86_64/cuda-keyring_1.1-1_all.deb
sudo dpkg -i /tmp/cuda-keyring.deb
sudo apt update
```

Install cuda

```
sudo apt install -y cuda
sudo apt autoremove
sudo apt clean
sudo reboot
```

Check the driver is working

```
nvidia-smi
```

Install openCL

```
sudo apt install -y nvidia-opencl-dev clinfo
```

Check openCL is working

```
clinfo
```

## Digital Research Alliance of Canada cloud

Free for academics in Canada: [info](https://docs.alliancecan.ca/wiki/Cloud)

Start an instance with

- `g1-8gb-c4-22gb`
- Ubuntu


Update and reboot

```
sudo apt update
sudo apt dist-upgrade
sudo reboot
```

Add the repository for GPU drivers

```
wget -O /tmp/arbutus-cloud-repo_all.deb http://repo.arbutus.cloud.computecanada.ca/pulp/deb/ubuntu22/pool/main/arbutus-cloud-repo_0.2_all.deb
sudo dpkg -i /tmp/arbutus-cloud-repo_all.deb 
```

Install drivers

```
sudo apt --yes install nvidia-vgpu-kmod nvidia-vgpu-tools nvidia-vgpu-gridd
sudo reboot
```

Check the driver is working

```
nvidia-smi
```

Install openCL

```
sudo apt-get install --yes opencl-headers clinfo ocl-icd-opencl-dev 
```

Check openCL is working

```
clinfo
```



# R and packages


Add R repositories

```
sudo add-apt-repository --yes "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository --yes ppa:c2d4u.team/c2d4u4.0+
```

Install R

```
sudo apt --yes install r-cran-devtools r-cran-rcppeigen r-cran-bh r-cran-testthat r-cran-knitr r-cran-assertive
sudo apt-get clean
```

Set up some folders for R, including a personal library.  

```
mkdir ~/.R 
echo 'MAKEFLAGS = -j4' > ~/.R/Makevars
R -e 'dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)'
```

Install the GPU packages!

```
devtools::install_github("eborgnine/gpuR")
devtools::install_github("ruoyongxu/clrng")
 ```

# openCL on CPU

It is possible to install openCL for use with a CPU rather than a GPU.  This could be useful for development and testing, but the code will run considerably slower than on a GPU.


```
sudo apt install intel-opencl-icd
sudo apt install opencl-dev clinfo
clinfo
```

```
library('gpuR')
listContexts()
setContext(grep("cpu", listContexts()$device_type)[1])
```
