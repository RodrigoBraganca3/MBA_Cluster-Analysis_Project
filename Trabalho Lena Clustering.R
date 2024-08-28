#### Leitura de arquivos de imagem ####
install.packages("jpge")
install.packages("png")
install.packages("grid")
install.packages("gridExtra")


library("ggplot2")
library("grid")
library("gridExtra")
library("png")
library("cluster")



#### Leitura de png ####

getwd()
setwd("C:/Users/rodri/OneDrive/Documentos/RodrigoBragança_Análise de clusters [23E3_2]")
Lena_Image_Raw = readPNG("Lena_Cluster.png")


# Show the full RGB image # Dimension = 512 512   3 #
grid.raster(Lena_Image_Raw)
dim(Lena_Image_Raw)


# Show the B channel in gray scale representing pixel intensity
grid.raster(Lena_Image_Raw[,,3])

# Show the 3 channels in separate images
# copy the image three times
Lena.R = Lena_Image_Raw
Lena.G = Lena_Image_Raw
Lena.B = Lena_Image_Raw

# zero out the non-contributing channels for each image copy
Lena.R[,,2:3] = 0
Lena.G[,,1]=0
Lena.G[,,3]=0
Lena.B[,,1:2]=0

# build the image grid
img0 = rasterGrob(Lena_Image_Raw)
img1 = rasterGrob(Lena.R)
img2 = rasterGrob(Lena.G)
img3 = rasterGrob(Lena.B)
grid.arrange(img0, img1, img2, img3, nrow=1)


#### CLARA ####

# Reshape image into a data frame
ima_df = data.frame(
  red = matrix(Lena_Image_Raw[,,1], ncol = 1),
  green = matrix(Lena_Image_Raw[,,2], ncol = 1),
  blue = matrix(Lena_Image_Raw[,,3], ncol = 1)
)

# Perform CLARA clustering with 3 clusters
num_clusters = 3
clustering = clara(ima_df, num_clusters, samples = 1000)



# Get cluster assignments
cluster_assignments = clustering$clustering


# Get cluster medoids (centers)
cluster_medoids <- clustering$medoids

ima_df[,"red"] = cluster_medoids[cluster_assignments, "red"]
ima_df[,"green"] = cluster_medoids[cluster_assignments, "green"]
ima_df[,"blue"] = cluster_medoids[cluster_assignments, "blue"]

# Reshape the segmented image data
R <- matrix(ima_df$red, nrow = dim(Lena_Image_Raw)[1])
G <- matrix(ima_df$green, nrow = dim(Lena_Image_Raw)[1])
B <- matrix(ima_df$blue, nrow = dim(Lena_Image_Raw)[1])

# Reconstitute the segmented image in the same shape as the input image
Lena_segmented <- array(dim = dim(Lena_Image_Raw))
Lena_segmented[,,1] <- R
Lena_segmented[,,2] <- G
Lena_segmented[,,3] <- B

# Display the original image and the segmented result
grid.arrange(
  rasterGrob(Lena_Image_Raw),
  rasterGrob(Lena_segmented),
  ncol = 2
)

#### CLARA (3,5,10 clusters) ####

# Reshape image into a data frame
ima_df = data.frame(
  red = matrix(Lena_Image_Raw[,,1], ncol = 1),
  green = matrix(Lena_Image_Raw[,,2], ncol = 1),
  blue = matrix(Lena_Image_Raw[,,3], ncol = 1)
)

# Perform CLARA clustering with 3 clusters
num_clusters = 10
clustering = clara(ima_df, num_clusters, samples = 1000)



# Get cluster assignments
cluster_assignments = clustering$clustering


# Get cluster medoids (centers)
cluster_medoids <- clustering$medoids

ima_df[,"red"] = cluster_medoids[cluster_assignments, "red"]
ima_df[,"green"] = cluster_medoids[cluster_assignments, "green"]
ima_df[,"blue"] = cluster_medoids[cluster_assignments, "blue"]

# Reshape the segmented image data
R = matrix(ima_df$red, nrow = dim(Lena_Image_Raw)[1])
G = matrix(ima_df$green, nrow = dim(Lena_Image_Raw)[1])
B = matrix(ima_df$blue, nrow = dim(Lena_Image_Raw)[1])

# Reconstitute the segmented image in the same shape as the input image
Lena_segmented = array(dim = dim(Lena_Image_Raw))
Lena_segmented[,,1] = R
Lena_segmented[,,2] = G
Lena_segmented[,,3] = B

# Display the original image and the segmented result
grid.arrange(
  rasterGrob(Lena_Image_Raw),
  rasterGrob(Lena_segmented),
  ncol = 2
)
