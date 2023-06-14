# Installation


SCI-VCF can be installed in multiple ways!

## Online access
<br>

Step 1:  Open the [SCI-VCF](https://github.com/venkatk89/sum_vcf) website.

<br>

*Note: Online version of SCI-VCF comes with upload size limitations. To get the full functionalities of SCI-VCF, use a local/server installation of the tool.*

<br>

## RStudio based installation

**Prerequisites**
+ R (version > 4.2)
+ RStudio 
 
**Procedure**

Step 1: Get the SCI-VCF repository from GitHub. <br>
You can download the zipped version of the repo at this [link](https://github.com/venkatk89/SCI-VCF)

*Insert Image*

Step 2: Extract the files in your computer

*Insert image*

Step 3: Launch SCI-VCF

Open ```global.R``` with RStudio and click the ```Run App``` button.

*Insert Image*

*Note: The first time you launch SCI-VCF by clicking the ```Run App``` button, some dependencies will be downloaded. Kindly wait till then. You need to be connected to the internet and might need to give authorization if required. Once the dependencies are installed, SCI-VCF can be used offline thereafter.*

<br>

## Conda based installation

> For Linux/Unix systems

**Prerequisites**
+ Conda

**Procedure**

Step 1: Get the SCI-VCF repository from GitHub.
```
git clone https://github.com/venkatk89/SCI-VCF
```

Step 2: OPen the SCI-VCF directory
```
cd SCI-VCF
```

Step 3: Create the conda environment.
```
# For linux/unix systems
conda env create -f conda_installation/SCI-VCF_linux_conda_env.yaml
```

Step 4: Activate the conda environment.
```
conda activate SCI-VCF
```

Step 5: Open ```global.R```.
```
rstudio global.R
```

Step 5: Launch SCI-VCF by clicking the ```Run App``` button.


## Docker based installation

*Write Stuff here*