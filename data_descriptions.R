# datasets

datasets <- tibble(name = c('Developing Human Atlas',
                            'Intestine Duodenum - Epithelium',
                            'All Organs Mesenchyme',
                            'All Organs Mesenchyme (Age/type)',
                            'All Organs Epithelium (Age/type)',
                            'All Organs - Peripheral Nervous System',
                            'Colon Epithelium (Age/type)',
                            'Colon Mesenchyme (Age/type)',
                            'Esophagus Epithelium',
                            'Esophagus Epithelium (Age/type)',
                            'Esophagus Mesenchyme (Age/type)',
                            'Intestine Epithelium',
                            'Small Intestine Epithelium (Age/type)',
                            'Small Intestine Mesenchyme (Age/type)',
                            'Lung Epithelium',
                            'Lung Epithelium (Age/type)',
                            'Lung Mesenchyme (Age/type)',
                            'Stomach Epithelium',
                            'Stomach Epithelium (Age/type)',
                            'Stomach Mesenchyme (Age/type)'),
                   file = c('Res_fetal_atlas_new_2.rds',
                            'Res_tHIO_fetal_and_adult_duodenum_epi_integrated_with_CSS_new_2.rds',
                            "Res_fetal_multi_organ_meso_with_CSS_and_ct_by_organ_group.rds",
                            
                            "Res_fetal_age_and_cell_type_selected_mes_all_organ_combined.rds",
                            'Res_fetal_age_and_cell_type_selected_epi_all_organ_combined.rds',
                            'Res_PNS_all_organ_combined.rds',
                            
                            "Res_fetal_Colon_selected_age_and_cell_type_epithelium.rds",        
                            "Res_fetal_Colon_selected_age_and_cell_type_mesenchyme.rds",
                            "Res_fetal_esophagus_epithelium.rds",                               
                            "Res_fetal_Esophagus_selected_age_and_cell_type_epithelium.rds",    
                            "Res_fetal_Esophagus_selected_age_and_cell_type_mesenchyme.rds",    
                            "Res_fetal_intestine_epithelium.rds",  
                            "Res_fetal_SI_selected_age_and_cell_type_epithelium.rds",           
                            "Res_fetal_SI_selected_age_and_cell_type_mesenchyme.rds",
                            "Res_fetal_lung_epithelium.rds",                                    
                            "Res_fetal_Lung_selected_age_and_cell_type_epithelium.rds",         
                            "Res_fetal_Lung_selected_age_and_cell_type_mesenchyme.rds",
                            "Res_fetal_stomach_epithelium.rds",                                 
                            "Res_fetal_Stomach_selected_age_and_cell_type_epithelium.rds",      
                            "Res_fetal_Stomach_selected_age_and_cell_type_mesenchyme.rds"))



# description of datasets

descriptions <- list(`Developing Human Atlas` = p('A developing human multi-organ reference cell atlas focusing on endoderm-derived organs which was created 
                                                  by integrating newly generated and published single-cell transcriptomes from lung, esophagus,
                                                  liver, stomach, small intestine (duodenum, jejunum, ileum), and colon with an age distribution spanning
                                                  from 7 to 21 post-conception weeks. All of the developing human scRNA-seq data were
                                                  integrated using Cluster Similarity Spectrum (CSS) to remove confounding effects of random fluctuation
                                                  or technical differences among samples. For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                  cell type are available.',style = "text-align: justify"),
                     `Intestine Duodenum - Epithelium` = p('This dataset describes the developing human instestine duodenum. UMAP embeddings are integrated from tHIO, developing 
                                                           and adult duodenum epithelial scRNA-seq datasets. This dataset can also be viewed in 3D. 
                                                           For this dataset annotations for gene expression, age, organ, tissue and cell type are available.',style = "text-align: justify"),
                     `All Organs Epithelium (Age/type)` = p('This dataset contains a UMAP embedding of epithelial cells from the developing human atlas. The dataset was corrected for cell type and age.
                                                 For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                  cell type are available.',style = "text-align: justify"),
                     `All Organs Mesenchyme (Age/type)` = p('This dataset contains a UMAP embedding of mesenchymal cells from the developing human atlas. The dataset was corrected for cell type and age.
                                                 For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                  cell type are available.',style = "text-align: justify"),
                     `All Organs - Peripheral Nervous System` = p('This dataset contains a UMAP embedding of the peripheral nervous system from the developing human atlas.
                                                 For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                  cell type are available.',style = "text-align: justify"),
                     `All Organs Mesenchyme` = p('This dataset contains a UMAP embedding of mesenchymal cells from the developing human atlas.
                                                 For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                  cell type are available.',style = "text-align: justify"),
                     `Colon Epithelium (Age/type)` = p('This dataset contains a UMAP embedding of epithelial colon cells from the developing human atlas.
                                                 For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                  cell type are available and cells were selected for and cell type.',style = "text-align: justify"),
                     `Colon Mesenchyme (Age/type)` = p('This dataset contains a UMAP embedding of mesenchymal colon cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available and cells were selected for age and cell type.',style = "text-align: justify"),
                     `Esophagus Epithelium` = p('This dataset contains a UMAP embedding of epithelial esophagus cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available.',style = "text-align: justify"),
                     `Esophagus Epithelium (Age/type)` = p('This dataset contains a UMAP embedding of epithelial esophagus cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available and cells were selected for age and cell type.',style = "text-align: justify"),
                     `Esophagus Mesenchyme (Age/type)` = p('This dataset contains a UMAP embedding of mesenchymal esophagus cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available and cells were selected for age and cell type.',style = "text-align: justify"),
                     `Intestine Epithelium` = p('This dataset contains a UMAP embedding of epithelial intestine cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available.',style = "text-align: justify"),
                     `Small Intestine Epithelium (Age/type)` = p('This dataset contains a UMAP embedding of epithelial small intestine cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available and cells were selected for age and cell type.',style = "text-align: justify"),
                     `Small Intestine Mesenchyme (Age/type)` = p('This dataset contains a UMAP embedding of mesenchymal small intestine cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available and cells were selected for age and cell type.',style = "text-align: justify"),
                     `Lung Epithelium` = p('This dataset contains a UMAP embedding of epithelial lung cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available.',style = "text-align: justify"),
                     `Lung Epithelium (Age/type)` = p('This dataset contains a UMAP embedding of epithelial lung cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available and cells were selected for age and cell type.',style = "text-align: justify"),
                     `Lung Mesenchyme (Age/type)` = p('This dataset contains a UMAP embedding of mesenchymal lung cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available and cells were selected for age and cell type.',style = "text-align: justify"),
                     `Stomach Epithelium` = p('This dataset contains a UMAP embedding of epithelial stomach cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available.',style = "text-align: justify"),
                     `Stomach Epithelium (Age/type)` = p('This dataset contains a UMAP embedding of epithelial stomach cells from the developing human atlas.
                                                                       For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                       cell type are available and cells were selected for age and cell type.',style = "text-align: justify"),
                     `Stomach Mesenchyme (Age/type)` = p('This dataset contains a UMAP embedding of mesenchymal stomach cells from the developing human atlas.
                                                                     For this dataset annotations for gene expression, age, organ, tissue, major cell type and 
                                                                     cell type are available and cells were selected for age and cell type.',style = "text-align: justify"))












