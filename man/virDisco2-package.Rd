\name{virDisco2-package}
\alias{virDisco2-package}
\title{This is a short tutorial with an example of how to use this package.}
\description{
The execution order of the functions of the analysis pipeline is as follows:

\itemize{
\item \code{\link{create_directories_and_file_paths}}
\item \code{\link{extract_information_from_reference_genome}}
\item \code{\link{consensus_sequence}}
\item \code{\link{sim_init}} with parameter \code{type_of_sam_file = "init"}
\item \code{\link{sim_init_graphics}}
\item \code{\link{sim_fastq}}
\item \code{\link{mapping_bowtie2}}
\item \code{\link{error_rates}}
\item \code{\link{sim_init}} with parameter \code{type_of_sam_file = "sim"}
\item \code{\link{sim_sam_graphics}}
}

}

\examples{

########## Create directories and file paths ##########

# To prepare the simulation, the user needs to specify the directory containing the sam-files
# and the directory containing the excel result files from the prior mapping results.
# The simulation result files will be stored in the
# metasimulations directory which the user must specify, too.
# The R-function "create_directories_and_file_paths" creates subdirectories in the metasimulations directory
# and stores the directories and file paths needed later as global variables.

# Create new folders "sam_directory", "excel_directory" and "metasimulations_directory" at the desktop,
# extract the sam-file to the sam-directory and copy the excel-file to the excel-directory.

{
if ( Sys.info()[1] == "Windows") {
desktop.directory = file.path(Sys.getenv("USERPROFILE"))
desktop.directory = gsub("\\\\\\\\","/",desktop.directory)
desktop.directory = paste0(desktop.directory,"/Desktop")
my.sam.directory = paste0(desktop.directory,"/sam_directory")
my.excel.directory = paste0(desktop.directory,"/excel_directory")
my.metasimulations.directory = paste0(desktop.directory,"/metasimulations_directory")
}
else if ( Sys.info()[1] == "Linux") {
desktop.directory=system("xdg-user-dir DESKTOP",intern = T)
my.sam.directory = paste0(desktop.directory,"/sam_directory")
my.excel.directory = paste0(desktop.directory,"/excel_directory")
my.metasimulations.directory = paste0(desktop.directory,"/metasimulations_directory")
}
else {
my.sam.directory = file.choose() # Choose sam directory
my.excel.directory = file.choose() # Choose excel directory
my.metasimulations.directory = file.choose() # Choose metasimulations directory
}
}
dir.create(my.sam.directory)
dir.create(my.excel.directory)
unzip(zipfile = "./inst/extdata/NGS-12345_mapq_filtered.zip",exdir = my.sam.directory)
file.copy(from = "./inst/extdata/NGS-12345.xlsx",to = my.excel.directory)
create_directories_and_file_paths ( sam.directory = my.sam.directory , excel.directory = my.excel.directory , metasimulations.directory = my.metasimulations.directory )

########## Extract information from reference genome ##########

# The function "extract_information_from_reference_genome"
# needs the file path to the formatted reference genome fasta-file
# (in which one sequence is stored in one line; in the original fasta-file
# there could be a line break after 80 characters)
# and extracts information about the NC numbers, species, sequences and sequence lengths.
# These information are stored as global variables for the simulation procedure afterwards.

extract_information_from_reference_genome(formatted.reference_genome_fasta.file_path = file.choose()) # Path of formatted reference genome fasta file

########## Calculate consensus sequence of all original reads ##########

# In order to visualise the simulation of read sequences, this function produces a four-coloured graphic
# in which the colours blue, green, red and yellow represent the nucleotides A, C, G and T.
# The read sequences of the original sam-file are plotted in rows
# whereupon each column corresponds to a base position of the reference genome.
# Below the mapped read sequences, the consensus sequence is displayed
# and at the bottom the subsection of the reference genome belonging to the reads.
# The graphic is stored in the temporary directory.

# Before using the function "consensus_sequence",
# the functions "create_directories_and_file_paths" and "extract_information_from_reference_genome" must be executed.

consensus_sequence ( i = 1 , j = 3 )

########## Simulation initialisation ##########

# Before the actual simulation procedure can start,
# statistical distributions of the original mapping results must be calculated by the function "sim_init".
# The statistical distributions from the sam-files will be stored in the RDS objects directory, for each NGS run separately.

# Before using the function "sim_init",
# the functions "create_directories_and_file_paths" and "extract_information_from_reference_genome" must be executed.

# sim_init(file_indices=1,type_of_sam_file="init") # Processing takes a few minutes of time!

########## Graphics of simulation initialisation ##########

# This function produces graphics based on the statistical distributions of the original sam-files
# which are generated by the function "sim_init".
# The function plots distributions of nucleotide, mapping quality, read length, start position and quality value
# of all mapped viruses together and top three mapped viruses
# and writes the plots into one pdf-file.

# Before using the function "sim_init_graphics",
# the functions "create_directories_and_file_paths", "extract_information_from_reference_genome" and "sim_init" must be executed.

sim_init_graphics ( file_indices = 1 )

########## Simulation of fastq-files ##########

# This function produces paired fastq-files based on the statistical distributions of the original sam-files
# which are generated by the function "sim_init".
# The simulated paired fastq-files will be stored in the fastq-directory.

# Do not use read_counts = "density" in combination with start_positions_and_read_lengths = "original"!
# Before using the function "sim_fastq",
# the functions "create_directories_and_file_paths", "extract_information_from_reference_genome" and "sim_init" must be executed.

sim_fastq ( file_indices = 1 , read_counts = "density", start_positions_and_read_lengths = "random", seed = 42 ) # Takes a few seconds to run!

########## Mapping of fastq-files ##########

# The simulated fastq-files are used to be mapped against the artificial viral reference genome with bowtie2.
# After this step, one can filter out reads with a low mapping quality.
# The new sam-file with filtered reads is stored separately in the same directory.

library(rChoiceDialogs)
my.prefix = rchoose.dir() # Choose prefix (folder) of reference genome bowtie2 index files
my.prefix = gsub("\\\\","/",my.prefix)
my.prefix = paste0(my.prefix,"/")
mapping_bowtie2 ( file_indices = 1 , reference_genome_index_bowtie2.directory = my.prefix , mapq_filter_threshold = 2 , threads = 2 ) # Takes a few seconds to run!

########## Calculation of mapping and error rates ##########

# In total, this function computes six different diagnostic parameters counting relative frequencies,
# two mapping rates and four error rates.
# Furthermore, there are three absolute frequencies which count all mapped reads,
# reads that are mapped to an excel list of viruses and reads that are mapped correctly.
# These values are calculated by using information of the mapping files of the simulated fastq-files
# from the previous step.
# The old excel-file from the original mapping results is completed by six columns for the diagnostic parameters
# and saved as a new excel-file.

error_rates ( file_indices = 1 )

########## Simulation initialisation ##########

# sim_init(file_indices=1,type_of_sam_file="sim") # Execute after mapping simulated fastq-files!

########## Graphics of simulated sam-files ##########

# This function produces graphics based on the statistical distributions of the original sam-files
# and the sam-files produced by mapping of the simulated fastq-files
# which are generated by the function "sim_init" with parameters "init" and "sim".
# The function plots distributions of nucleotide, mapping quality, read length, start position and quality value
# of all mapped viruses together and top three mapped viruses
# for both types of sam-files and writes the plots into one pdf-file.

# Before using the function "sim_init_graphics",
# the functions "create_directories_and_file_paths", "extract_information_from_reference_genome" and "sim_init"
# with parameters "init" and "sim" must be executed.

sim_sam_graphics ( file_indices = 1 )

}
