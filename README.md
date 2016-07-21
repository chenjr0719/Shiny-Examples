# Shiny-Examples

A Simple Implement of **Standard Statistics Test**, **Clustering**, **Classification** by using **Shiny**.

Live Demo: http://chenjr-jacob.idv.tw:3838/Shiny_Examples/

## What is Shiny?

**Shiny** is a web application framework for **R**.

It can help developer develop there interactive Analysis tools easier.

You don't have to learn **HTML**, **CSS**, **JavaScript**, and others.

## About This Examples?

This Examples implement such as **Standard Statistics Test**, **Clustering**, **Classification**.

You use some dataset which already build in **R** or you can upload your own **.CSV** files.

The detail is following:

### Standard Statistics Test

In **Standard Statistics Test**, it contains:

* Regression
* Paired T Test
* One-Way ANOVA
* MANOVA

### Clustering

In **Clustering**, it contains:

* K-Means
* EM
* DBSCAN
* Spectral

### Classification

In **Classification**, it contains:

* Decision Tree
* Random Forest
* K-Nearest Neighbors
* Support Vector Machine
* Naive Bayes Classifier
* Feed-Forward Neural Network

## How to Use?

If you want to use this example, you have to build a shiny server first.

Thanks for **Docker**, you can build shiny server in a simple way.

You can reference my posts: http://chenjr-jacob.idv.tw/2016/04/19/shiny-using-r-to-publish-your-own-data-analysis-tools/

Or execute following commands:

```s
sudo docker pull rocker/shiny

sudo docker run -p 3838:3838 -v $YOUR_APP_DIR:/srv/shiny-server/ -v $YOUR_LOG_DIR:/var/log/ -d rocker/shiny
```

After container is ready, you have to install requirement packages of **Shiny-Examples**

Run the command to enter container:

```s
sudo docker exec -it $CONYAINER_ID bash
```

And run next command in container:

```s
Rscript /srv/shiny-server/Shiny-Examples/PkdLoad.R
```

This script will install requirement packages automaticlly.

And now, you can check http://localhost:3838/Shiny-Examples to see the result.

