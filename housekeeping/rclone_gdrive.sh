## Script to keep in sync local results with Google Drive

#module load tools/rclone

# Store variables and paths of interest
path_to_dags_local="../DAGs/"
path_to_dags_remote="gdrive:PhD_lorenzo/home/output/.../DAGs"

path_to_roadmap_local="../docs/*.pdf"
path_to_roadmap_remote="gdrive:PhD_lorenzo/home/output/.../docs"

path_to_paper_local="../output/paper/manuscript.pdf"
path_to_paper_remote="gdrive:PhD_lorenzo/home/output/.../output/paper"

# TODO: posters, presentations #

path_to_figures_local="../results/figures/"
path_to_figures_remote="gdrive:PhD_lorenzo/home/output/.../results/figures"

path_to_tables_local="../results/tables/"
path_to_tables_remote="gdrive:PhD_lorenzo/home/output/.../results/tables"

# Specify which directories/files should be kept in sync with Google Drive
## DAGs
rclone sync $path_to_dags_local $path_to_dags_remote

## Causal roadmap
rclone sync $path_to_roadmap_local $path_to_roadmap_remote

## PDF of paper, posters, presentations
rclone copy $path_to_paper_local $path_to_paper_remote

## Figures, tables
rclone sync $path_to_figures_local $path_to_figures_remote
rclone sync $path_to_tables_local $path_to_tables_remote
