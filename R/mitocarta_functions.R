
#' Extract MitoCarta Data
#'
#' This function extract MitoCarta3.0 data from the web. This function also formats
#' columns for use with other functions in the `MitochondrialGeneAnalyses`.
#'
#' @param species One of `mouse` or `human` to define whether the function should 
#' extract the Human.MitoCarta3.0 information or the Mouse.MitoCarta3.0 information. 
#' Defaults to `human`. Note that gene symbols from either data set will integrate
#' with other mammalian species.
#' @param gene_id_type One of one of `entrez_id`, `gene_symbol`, or `ensembl`. All
#' data extracted from MitoCarta will be maintained, but this option formats column
#' names for consistent downstream analysis.
#' @importFrom magrittr "%>%"
#' @returns A list of dataframes containing gene-wise and pathway-wise information 
#' from MitoCarta
#' @source "https://personal.broadinstitute.org/scalvo/MitoCarta3.0" 
#' @export
extract_mitocarta_data <- function(species = "human",
                                   gene_id_type = "gene_symbol") {
  
  message("Extracting MitoCarta3.0 dataset")
  temp = tempfile(fileext = ".xls")
  
  if(species == "human"){
    dataURL <- "https://personal.broadinstitute.org/scalvo/MitoCarta3.0/Human.MitoCarta3.0.xls"
    utils::download.file(dataURL, destfile=temp, mode='wb')
    mito_pathways <- readxl::read_excel(temp, sheet = 4)
    all_genes <- readxl::read_excel(temp, sheet = 3)
    mito_genes <- readxl::read_excel(temp, sheet = 2)
    
    if(gene_id_type == "entrez_id"){
      mito_genes <- mito_genes %>%
        dplyr::mutate(Name = as.character(.data$HumanGeneID),
                      Pathways = .data$MitoCarta3.0_MitoPathways)
      all_genes <- all_genes %>%
        dplyr::mutate(Name = as.character(.data$HumanGeneID))
      
    } else if(gene_id_type == "gene_symbol"){
      mito_genes <- mito_genes %>%
        dplyr::mutate(Name = as.character(.data$Symbol),
                      Pathways = .data$MitoCarta3.0_MitoPathways)
      all_genes <- all_genes %>%
        dplyr::mutate(Name = as.character(.data$Symbol))
      
    } else if(gene_id_type == "ensembl"){
      mito_genes <- mito_genes %>%
        dplyr::mutate(Name = as.character(.data$EnsemblGeneID_mapping_version_20200130),
                      Pathways = .data$MitoCarta3.0_MitoPathways)
      all_genes <- all_genes %>%
        dplyr::mutate(Name = as.character(.data$EnsemblGeneID_mapping_version_20200130))
      
    } else {
      stop("gene_id_type must be one of 'entrez_id','gene_symbol',or 'ensembl'")
    }
    
  } else if(species == "mouse"){
    dataURL <- "https://personal.broadinstitute.org/scalvo/MitoCarta3.0/Mouse.MitoCarta3.0.xls"
    utils::download.file(dataURL, destfile=temp, mode='wb')
    mito_pathways <- readxl::read_excel(temp, sheet = 4)
    all_genes <- readxl::read_excel(temp, sheet = 3)
    mito_genes <- readxl::read_excel(temp, sheet = 2)
    
    if(gene_id_type == "entrez_id"){
      mito_genes <- mito_genes %>%
        dplyr::mutate(Name = as.character(.data$MouseGeneID),
                      Pathways = .data$MitoCarta3.0_MitoPathways)
      all_genes <- all_genes %>%
        dplyr::mutate(Name = as.character(.data$MouseGeneID))
      
    } else if(gene_id_type == "gene_symbol"){
      mito_genes <- mito_genes %>%
        dplyr::mutate(Name = as.character(.data$Symbol),
                      Pathways = .data$MitoCarta3.0_MitoPathways)
      all_genes <- all_genes %>%
        dplyr::mutate(Name = as.character(.data$Symbol))
      
    } else if(gene_id_type == "ensembl"){
      mito_genes <- mito_genes %>%
        dplyr::mutate(Name = as.character(.data$EnsemblGeneID),
                      Pathways = .data$MitoCarta3.0_MitoPathways)
      all_genes_formatted <- all_genes %>%
        dplyr::mutate(Name = as.character(.data$EnsemblGeneID))
      
    } else {
      stop("gene_id_type must be one of 'entrez_id','gene_symbol',or 'ensembl'")
    }
    
  } else {
    stop("species must be one of 'human' or 'mouse'")
  }
  
  return(l = list(MitoCarta_MitoGenes = mito_genes,
                  MitoCarta_AllGenes = all_genes,
                  MitoCarta_Pathways = mito_pathways
                  ))
}






#' Check MitoCarta Synonyms
#'
#' This function accepts a custom vector of `gene_symbols` and returns a parallel list of
#' genes with any necessary substitutions, should the gene name fall as a synonym
#' in the MitoCarta3.0 annotation. This is a cleaning step before pathway identification
#' to help match as many genes as possible. 
#'
#' @param gene_names A vector of user-provided gene names. The function will scan
#' the entire list of gene names to identify whether the gene name is listed as 
#' either a primary gene or synonym as annotated by MitoCarta3.0
#' @param mitocarta_data A list with three dataframe objects `MitoCartaGenes`, 
#' `AllGenes`, and `MitoCartaPathways` generated from `extract_mitocarta_data()` 
#' with or without formatting with `format_mitocarta_data()`
#' as long as `'gene_symbol'` was selected for `gene_id_type`.
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @returns A dataframe of gene names, where column 1 (`given_id`) is the provided 
#' `gene_names` and column 2 (`mitocarta_id`) is the is populated with the gene 
#' names as annotated by MitoCarta3.0.
#' @export
check_mitocarta_synonyms <- function(gene_names = NULL, 
                                     mitocarta_data = NULL) {
  
  # First, initialize new dataframe for storing eventual mitocarta_id
  new_gene_names <- data.frame(given_id = gene_names,
                               Name_check = toupper(gene_names))
  
  # Parse "MitoCarta_AllGenes" datafram from mitocarta_data, make uppercase, and merge
  # with gene_names to identify which ones have an ID match in MitoCarta dataset
  new_gene_names <- mitocarta_data[["MitoCarta_AllGenes"]] %>%
    dplyr::select(.data$Symbol) %>%
    dplyr::mutate(Name_check = toupper(.data$Symbol)) %>%
    dplyr::right_join(new_gene_names, by = "Name_check")
  
  # Pull names of genes that do not have a Symbol match in MitoCarta dataset
  names_to_check_synonyms <- new_gene_names %>%
    dplyr::filter(is.na(.data$Symbol)) %>%
    dplyr::pull(.data$Name_check)
  
  # Construct dataframe with synonyms from mitochondria genes only
  temp_synonyms1 <- mitocarta_data[["MitoCarta_MitoGenes"]] %>%
    dplyr::mutate(all_synonyms = stringr::str_split(.data$Synonyms, "\\|", n = Inf, simplify = F))

  temp_synonyms2 <- c()
  for(n in 1:length(temp_synonyms1$all_synonyms)) {
    temp_synonyms3 <- data.frame(Symbol = temp_synonyms1$Symbol[n],
                          Synonyms = temp_synonyms1$all_synonyms[[n]])
    temp_synonyms2 <- rbind(temp_synonyms2,temp_synonyms3)
  }
  
  # Merge add uppercase for checking without capitalization issues and filter 
  # for genes that need a synonym check
  temp_synonyms2 <- temp_synonyms2 %>%
    dplyr::mutate(Symbol_upper = toupper(.data$Symbol)) %>%
    dplyr::mutate(Synonyms_upper = toupper(.data$Synonyms)) %>%
    dplyr::filter(.data$Synonyms_upper %in% names_to_check_synonyms) 
  
  # Replace NA Symbol name with MitoCarta symbol IDed from synonym if present
  if(nrow(temp_synonyms2)>0){
  message(paste0(nrow(temp_synonyms2), " symbol names found from synonym IDs and are being replaced"))
    for(i in 1:nrow(temp_synonyms2)){
      
      synonym_upper <- temp_synonyms2$Synonyms_upper[i]
      symbol_use <- temp_synonyms2$Symbol[i]
      
      new_gene_names[new_gene_names$Name_check==synonym_upper,
                     "Symbol"] <- symbol_use
    }
  } else {message("No symbol names found from synonym IDs")}
  
  if(sum(is.na(new_gene_names$Symbol))>0){
    message(paste0(sum(is.na(new_gene_names$Symbol))," symbol names from given gene names have no MitoCarta match and will be returned as NAs"))
  } 
  
  return(data.frame(given_id = new_gene_names$given_id,
                               mitocarta_id = new_gene_names$Symbol))
}



#' Assign MitoCarta Pathways
#'
#' This function assigns and parses MitoCarta3.0 pathways to gene ids using the 
#' output from  `extract_mitocarta_data()`, and if desired, `check_mitocarta_synonyms()`.
#'
#' @param gene_names A vector of user-provided gene names which should match the 
#' format provided for the `gene_id_type` provided in `format_mitocarta_data()`. 
#' For gene symbols, `gene_names` should match the `mitocarta_id` column output
#' from `check_mitocarta_synonyms()`.
#' @param mitocarta_data A list with dataframe objects `MitoCarta_MitoGenes`, 
#' `MitoCarta_AllGenes`, and `MitoCarta_Pathways` generated from `extract_mitocarta_data()`
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @returns A list of dataframes of the same format as `extract_mitocarta_data()`
#' with an an added dataframe `Assign_Pathways` which has the following columns: 
#' \describe{
#'   \item{Name}{Gene name, which will not be unique if it is assigned to multiple pathways}
#'   \item{Pathway.Number}{If a gene is assigned to multiple pathways, this column 
#'   identifies the pathway number}
#'   \item{Main.Path}{Full pathway description}
#'   \item{SubPath1}{Primary pathway as derived from Main.Path}
#'   \item{SubPath2}{Secondary pathway as derived from Main.Path, if it exists}
#'   \item{SubPath3}{Tertiary pathway as derived from Main.Path, if it exists}
#' }
#' @export
assign_mitocarta_pathways <- function(gene_names = NULL, mitocarta_data = NULL) {
  
  mitocarta_data_assigned <- data.frame(Name = as.character(gene_names)) %>%
    dplyr::left_join(mitocarta_data[["MitoCarta_MitoGenes"]][,c("Name","Pathways")],
                     by = "Name")
  
  if(length(mitocarta_data_assigned$Pathways)==sum(is.na(mitocarta_data_assigned$Pathways))){
    stop("No genes matched with MitoCarta pathways; check that species and gene_id_type selection were correct")
  }
  
  if(sum(is.na(mitocarta_data_assigned$Pathways))>0){
    message(paste0(sum(is.na(mitocarta_data_assigned$Pathways)), " genes that were not assigned a mitochondrial pathway will be removed"))
  }
  
  if(nrow(dplyr::filter(mitocarta_data_assigned, .data$Pathways == "0"))>0){
    message(paste0(nrow(dplyr::filter(mitocarta_data_assigned, .data$Pathways == "0")), " pathways annotated with '0' will be removed"))
  }
  
  # Assign pathways within a dataframe
  return_object <- mitocarta_data_assigned %>%
    stats::na.omit(.data$Pathways) %>%
    dplyr::mutate(all_pathways = stringr::str_split(.data$Pathways, "\\|", n = Inf, simplify = F)) %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(Pathway1 = .data$all_pathways[1], Pathway2 = .data$all_pathways[2], 
                  Pathway3 = .data$all_pathways[3], Pathway4 = .data$all_pathways[4], 
                  Pathway5 = .data$all_pathways[5], Pathway6 = .data$all_pathways[6],
                  Pathway7 = .data$all_pathways[7])  %>%
    tidyr::gather(key = "Pathway.Number", value = "Main.Path", na.rm = TRUE, 
                  c("Pathway1","Pathway2","Pathway3","Pathway4","Pathway5","Pathway6","Pathway7")) %>%
    dplyr::filter(.data$Main.Path != "0") %>% 
    dplyr::mutate(Main.Path = stringr::str_trim(.data$Main.Path, "left")) %>%
    dplyr::mutate(Main.Path = stringr::str_trim(.data$Main.Path, "right")) %>%
    dplyr::select(-c(.data$all_pathways,.data$Pathways)) %>%
    dplyr::mutate(sub_pathways = stringr::str_split(.data$Main.Path, " > ", n = Inf, simplify = F)) %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(SubPath1 = .data$sub_pathways[1], SubPath2 = .data$sub_pathways[2], 
                  SubPath3 = .data$sub_pathways[3])  %>%
    dplyr::select(-c(.data$sub_pathways))
  
  mitocarta_data[["Assigned_Pathways"]] <- return_object
  return(mitocarta_data)
}




#' Plot MitoCarta Pathways
#'
#' This function makes bar graphs indicated the number of genes in various MitoCarta
#' pathways that are represented in a filtered dataset from the `assign_mitocarta_pathways()`
#' output.
#'
#' @param filtered_data A dataframe with colnames `Name`, `SubPath1`, `SubPath2`, 
#' and `SubPath3` as generated from function `assign_mitocarta_pathways()` in the 
#' `Assigned_Pathways` dataframe
#' @param mitocarta_data A list with dataframe objects `MitoCarta_MitoGenes`, 
#' `MitoCarta_AllGenes`, `MitoCarta_Pathways`, and `Assigned_Pathways` generated 
#' from `extract_mitocarta_data()` and then `assign_mitocarta_data()`.
#' @param subpath One of `Primary` to visualize high-level pathways, or a primary
#' or secondary pathway name to indicate which MitoCarta pathway to parse 
#' (i.e. `Metabolism` or `Metals and cofactors`). Defaults to `Primary`. Is case
#' sensitive.
#' @param fc_name A character string indicating the column name in
#' `filtered_data` containing the logFC values to plot for the percent
#' pathway bar parts. If `NULL`, plot will not be populated with logFC colors.
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @returns A list of ggplot image objects
#' @export
plot_mitocarta_pathways <- function(filtered_data = NULL,
                                    mitocarta_data = NULL, 
                                    subpath = "Primary",
                                    fc_name = NULL) {
  
  if(is.null(filtered_data)){
    stop("Missing input data (filtered_data)")
  }
  if(is.null(mitocarta_data)){
    stop("Missing mitocarta_data list object")
  }
  
  if(!all(c("Name","SubPath1","SubPath2","SubPath3") %in% colnames(filtered_data))){
    stop("Input data object filtered_data does not contain all necessary columns
         'Name', 'SubPath1', 'SubPath2', and 'SubPath3' found in the dataframe 
         'Assigned_Pathways' generated from function assign_mitocarta_pathways()")
  }
  
  # Set parameters based on input subpath option
  if(subpath == "Primary"){
    plot_mitocarta_use <- filtered_data %>%
      dplyr::rename(SubPath_groupby = .data$SubPath1)
    
  } else if(subpath %in% unique(filtered_data$SubPath1)){
    plot_mitocarta_use <- filtered_data %>%
      dplyr::rename(SubPath_groupby = .data$SubPath2) %>%
      dplyr::filter(.data$SubPath1 == subpath)
    
  } else if(subpath %in% unique(filtered_data$SubPath2)){
    plot_mitocarta_use <- filtered_data %>%
      dplyr::rename(SubPath_groupby = .data$SubPath3) %>%
      dplyr::filter(.data$SubPath2 == subpath)
    
  } else if(subpath %in% unique(filtered_data$SubPath3)){
    stop("Cannot parse tertiary pathways. Please list a primary or secondary pathway.")
    
  } else {
    stop("Provided subpath not detected as an exact match amongst MitoCarta pathways. Please check spelling or case.")
  }
  
  if(nrow(plot_mitocarta_use)==0){
    stop(paste0("No genes in provided input dataframe filtered_data for subpath ", subpath))
  }
  
  # Data needs to be converted to long format for plotting in ggplot
  # For each subpath-gene group, only take one entry so that data is not redundant
  subpath_filtered <- plot_mitocarta_use %>%
    dplyr::group_by(.data$SubPath_groupby, .data$Name) %>%
    dplyr::slice_head(n = 1)
  
  if(nrow(subpath_filtered) < 3){
    stop(paste0("Less than 3 genes in in filtered_data for subpath ", subpath,"; not plotting"))
  }
  
  # Create a summary table of subpath data
  subpath_summary <- as.data.frame(table(subpath_filtered$SubPath_groupby)) 
  colnames(subpath_summary) <- c("Pathway","Freq")
  rownames(subpath_summary) <- subpath_summary$Pathway
  
  # Extract log2FC data
  if(!is.null(fc_name)) {
  log2_fc_data <- subpath_filtered %>%
    dplyr::group_by(.data$SubPath_groupby) %>%
    dplyr::mutate(log2FC_mean = mean(.data[[fc_name]], na.rm = T)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::select(.data$SubPath_groupby, .data$log2FC_mean)
  colnames(log2_fc_data)[1] <- "Pathway"
  
  } else {
    log2_fc_data <- subpath_filtered %>%
      dplyr::group_by(.data$SubPath_groupby) %>%
      dplyr::mutate(log2FC_mean = -1) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::select(.data$SubPath_groupby, .data$log2FC_mean)
    colnames(log2_fc_data)[1] <- "Pathway"
  }
  
  if(!all(c("MitoCarta_Pathways", "MitoCarta_AllGenes") %in% names(mitocarta_data))){
    stop("Incorrect data format for mitocarta_data; list objects 'MitoCarta_Pathways' and 'MitoCarta_AllGenes' missing")
  }
  
  # Calculate percent of each pathway is represented in our dataset
  for(pathway in unique(subpath_summary$Pathway)) {
    
    pathway_genes <- mitocarta_data[["MitoCarta_Pathways"]] %>%
      dplyr::filter(.data$MitoPathway == pathway) %>%
      dplyr::mutate(Genes = stringr::str_split(.data$Genes, ",", n = Inf, simplify = F)) %>% 
      dplyr::pull(.data$Genes)
    no_pathway_genes_in_my_datasets <- length(pathway_genes[[1]])
    
    # Add percent column
    subpath_summary[pathway, "Percent"] <- round(subpath_summary[pathway, "Freq"]/no_pathway_genes_in_my_datasets*100,2)
  }
  
  # Add log2FC information
  # Add information about length of y-axis names, for setting graph aspect ratio
  # Arrange Pathway names as levels in order of highest to lowest gene count frequency
  subpath_summary <- subpath_summary %>% 
    dplyr::left_join(log2_fc_data, by = "Pathway") %>% 
    dplyr::mutate(ylength = nchar(as.character(subpath_summary$Pathway))) %>%
    dplyr::arrange(.data$Freq) %>% 
    dplyr::mutate(Pathway = factor(.data$Pathway, as.character(.data$Pathway)))
  
  return_images <- list()
  
  # Graph number of genes in each pathway    
  n <- ggplot2::ggplot(data = subpath_summary, 
                       ggplot2::aes(x = .data$Pathway, y = .data$Freq)) + 
    ggplot2::geom_col(fill = "grey30") + 
    ggplot2::geom_text(ggplot2::aes(label = .data$Freq), hjust = 1.3, color = "white") +
    ggplot2::xlab("") + 
    ggplot2::ylab("Number of genes") +
    ggplot2::ggtitle(paste0(subpath)) +
    ggplot2::theme(axis.line = ggplot2::element_line(colour="black"),
                   panel.background = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size=12, color = "black"),
                   panel.grid.major = ggplot2::element_blank()) + 
    ggplot2::coord_flip()
  
  # Graph percent of genes represented in each pathway
  p <- ggplot2::ggplot(data = subpath_summary, 
                       ggplot2::aes(x = .data$Pathway, y = .data$Percent)) + 
    ggplot2::geom_col(ggplot2::aes(fill = .data$log2FC_mean)) + 
    ggplot2::scale_fill_gradient2("Log2FC",low="darkgreen", mid="white", high="firebrick", midpoint=0, limits = c(-1,1)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Percent), hjust = -0.2, color = "black") +
    ggplot2::xlab("") + 
    ggplot2::ylim(0,119) +
    ggplot2::ylab("% of genes in pathway") +
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "grey") +
    ggplot2::ggtitle(paste0(subpath)) +
    ggplot2::theme(axis.line = ggplot2::element_line(colour="black"),
                   panel.background = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size=12, color = "black"),
                   panel.grid.major = ggplot2::element_blank()) + 
    ggplot2::coord_flip() 
    
    return_images[["GeneNumber"]] <- n
    return_images[["PathwayPercent"]] <- p

  return(return_images)
}