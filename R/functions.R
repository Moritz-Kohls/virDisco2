#' Create directories and file paths
#'
#' To prepare the simulation, the user needs to specify the directory containing the sam-files
#' and the directory containing the excel result files from
#' the prior mapping results. The simulation result files will be stored in the
#' metasimulations directory which the user must specify, too. The R-function
#' \code{create_directories_and_file_paths} creates subdirectories in the metasimulations directory
#' and stores the directories and file paths needed later as global variables.
#'
#' @param sam.directory Directory of the sam-files with original mapping results
#' @param excel.directory Directory of the excel-files with mapped viruses information
#' @param metasimulations.directory Directory of the files produced by simulation procedure
#'
#' @return None
#'
#' @examples
#' # Create new folders "sam_directory", "excel_directory" and "metasimulations_directory" at the desktop,
#' # extract the sam-file to the sam-directory and copy the excel-file to the excel-directory.
#' {
#' if ( Sys.info()[1] == "Windows") {
#' desktop.directory = file.path(Sys.getenv("USERPROFILE"))
#' desktop.directory = gsub("\\\\","/",desktop.directory)
#' desktop.directory = paste0(desktop.directory,"/Desktop")
#' my.sam.directory = paste0(desktop.directory,"/sam_directory")
#' my.excel.directory = paste0(desktop.directory,"/excel_directory")
#' my.metasimulations.directory = paste0(desktop.directory,"/metasimulations_directory")
#' }
#' else if ( Sys.info()[1] == "Linux") {
#' desktop.directory=system("xdg-user-dir DESKTOP",intern = T)
#' my.sam.directory = paste0(desktop.directory,"/sam_directory")
#' my.excel.directory = paste0(desktop.directory,"/excel_directory")
#' my.metasimulations.directory = paste0(desktop.directory,"/metasimulations_directory")
#' }
#' else {
#' my.sam.directory = file.choose() # Choose sam directory
#' my.excel.directory = file.choose() # Choose excel directory
#' my.metasimulations.directory = file.choose() # Choose metasimulations directory
#' }
#' }
#' dir.create(my.sam.directory)
#' dir.create(my.excel.directory)
#' unzip(zipfile = "./inst/extdata/NGS-12345_mapq_filtered.zip",exdir = my.sam.directory)
#' file.copy(from = "./inst/extdata/NGS-12345.xlsx",to = my.excel.directory)
#' create_directories_and_file_paths ( sam.directory = my.sam.directory , excel.directory = my.excel.directory , metasimulations.directory = my.metasimulations.directory )
#'
#' @export
create_directories_and_file_paths = function ( sam.directory , excel.directory , metasimulations.directory ) {

  # Sam files and file paths from mapping:
  all.sam.files = list.files(path=sam.directory,pattern=".sam",recursive=T)
  indices = grep("mapq_filtered",all.sam.files)
  sam.files = all.sam.files [-indices]
  sam.mapq_filtered.files = all.sam.files [indices]

  sam.file_paths <<- paste0(sam.directory,sam.files)
  sam.mapq_filtered.file_paths <<- paste(sam.directory,sam.mapq_filtered.files,sep="/")

  # Excel result files and file paths after mapping:
  excel.files = list.files(path=excel.directory,pattern=".xlsx",recursive=T)
  indices.excel.files.error_rates = grep("error_rates|mapped_outside_of_list",excel.files)
  if ( length(indices.excel.files.error_rates) > 0 ) {
    excel.files = excel.files [-indices.excel.files.error_rates]
  }
  excel.file_paths <<- paste(excel.directory,excel.files,sep="/")

  # NGS numbers:
  indices.slash = regexpr("/",sam.mapq_filtered.files)
  indices.sam = regexpr(".sam",sam.mapq_filtered.files)
  indices.min = pmin(indices.slash,indices.sam)
  my.indices = which(indices.min == -1)
  indices.min [my.indices] = pmax(indices.slash[my.indices],indices.sam[my.indices])
  NGS_numbers = substring(sam.mapq_filtered.files,1,indices.min-1)
  NGS_numbers = gsub("_mapq_filtered","",NGS_numbers)

  # New simulation results:

  # Directories:
  #metasimulations.subdirectory = paste0(metasimulations.directory,"/bowtie2/")
  metasimulations.subdirectory = paste0(metasimulations.directory,"/")
  metasimulations.temp.directory <<- paste0(metasimulations.subdirectory,"temp/")
  metasimulations.fastq.directory = paste0(metasimulations.subdirectory,"fastq/")
  metasimulations.rds_objects.directory = paste0(metasimulations.subdirectory,"RDS_objects/")
  metasimulations.rds_objects.sim_init.directory = paste0(metasimulations.rds_objects.directory,"Sim_Init/")
  metasimulations.rds_objects.sim_sam.directory = paste0(metasimulations.rds_objects.directory,"Sim_Sam/")
  metasimulations.rds_objects.sim_init.directories <<- paste0(metasimulations.rds_objects.sim_init.directory,NGS_numbers,"/")
  metasimulations.rds_objects.sim_sam.directories <<- paste0(metasimulations.rds_objects.sim_sam.directory,NGS_numbers,"/")
  metasimulations_sam.directory <<- paste0(metasimulations.subdirectory,"Mapping_Sam/")
  metasimulations.graphics.directory = paste0(metasimulations.subdirectory,"Graphics/")

  # If they do not already exist, create new folders:
  if ( dir.exists(metasimulations.directory) == F ) dir.create(metasimulations.directory)
  if ( dir.exists(metasimulations.subdirectory) == F ) dir.create(metasimulations.subdirectory)
  if ( dir.exists(metasimulations.temp.directory) == F ) dir.create(metasimulations.temp.directory)
  if ( dir.exists(metasimulations.rds_objects.directory) == F ) dir.create(metasimulations.rds_objects.directory)
  if ( dir.exists(metasimulations.rds_objects.sim_init.directory) == F ) dir.create(metasimulations.rds_objects.sim_init.directory)
  if ( dir.exists(metasimulations.rds_objects.sim_sam.directory) == F ) dir.create(metasimulations.rds_objects.sim_sam.directory)
  for ( i in 1:length(metasimulations.rds_objects.sim_init.directories) ) {
    if ( dir.exists(metasimulations.rds_objects.sim_init.directories[i]) == F ) dir.create(metasimulations.rds_objects.sim_init.directories[i])
  }
  for ( i in 1:length(metasimulations.rds_objects.sim_sam.directories) ) {
    if ( dir.exists(metasimulations.rds_objects.sim_sam.directories[i]) == F ) dir.create(metasimulations.rds_objects.sim_sam.directories[i])
  }
  if ( dir.exists(metasimulations.fastq.directory) == F ) dir.create(metasimulations.fastq.directory)
  if ( dir.exists(metasimulations_sam.directory) == F ) dir.create(metasimulations_sam.directory)
  if ( dir.exists(metasimulations.graphics.directory) == F ) dir.create(metasimulations.graphics.directory)

  # fastq files and file paths:
  metasimulations.fastq_left.file_paths <<- paste0(metasimulations.fastq.directory,NGS_numbers,"_L.fastq")
  metasimulations.fastq_right.file_paths <<- paste0(metasimulations.fastq.directory,NGS_numbers,"_R.fastq")
  metasimulations.fastq_left.file_paths <<- gsub("_mapq_filtered","",metasimulations.fastq_left.file_paths)
  metasimulations.fastq_right.file_paths <<- gsub("_mapq_filtered","",metasimulations.fastq_right.file_paths)

  # Sam files from mapping:
  metasimulations_sam.files = paste0(NGS_numbers,".sam")
  metasimulations_sam.mapq_filtered.files  = paste0(NGS_numbers,"_mapq_filtered.sam")

  # Sam file paths from mapping:
  metasimulations_sam.file_paths <<- paste0(metasimulations_sam.directory,metasimulations_sam.files)
  metasimulations_sam.mapq_filtered.file_paths <<- paste0(metasimulations_sam.directory,metasimulations_sam.mapq_filtered.files)

  # Graphics directory and file paths:
  metasimulations.graphics_sim_init.file_paths <<- paste0(metasimulations.graphics.directory,"sim_init_graphics.",NGS_numbers,".pdf")
  metasimulations.graphics_sim_init.file_paths <<- gsub("_mapq_filtered","",metasimulations.graphics_sim_init.file_paths)
  metasimulations.graphics_sim_sam.file_paths <<- paste0(metasimulations.graphics.directory,"sim_sam_graphics.",NGS_numbers,".pdf")
  metasimulations.graphics_sim_sam.file_paths <<- gsub("_mapq_filtered","",metasimulations.graphics_sim_sam.file_paths)
  metasimulations.graphics_sim_mapping.file_paths <<- paste0(metasimulations.graphics.directory,"sim_mapping_graphics.",NGS_numbers,".csv")

}

#' Extract information from reference genome
#'
#' The function \code{extract_information_from_reference_genome}
#' needs the file path to the formatted reference genome fasta-file
#' (in which one sequence is stored in one line; in the original fasta-file
#' there could be a line break after 80 characters) and extracts information
#' about the NC numbers, species, sequences and sequence lengths.
#' These information are stored as global variables for the simulation procedure afterwards.
#'
#' @param formatted.reference_genome_fasta.file_path File path to formatted reference genome fasta file
#'
#' @return None
#'
#' @examples
#' extract_information_from_reference_genome(formatted.reference_genome_fasta.file_path = file.choose()) # Path of formatted reference genome fasta file
#' @export
extract_information_from_reference_genome = function ( formatted.reference_genome_fasta.file_path ) {
  reference_genome_fasta = readLines(formatted.reference_genome_fasta.file_path) # Formatted version
  temp = reference_genome_fasta[seq(1,length(reference_genome_fasta),by=2)]
  temp.sequences = reference_genome_fasta[seq(2,length(reference_genome_fasta),by=2)]
  indices.comma = regexpr(",",temp)
  reference_genome.NC_numbers_species <<- substring(temp,2,indices.comma-1)
  indices.blank = regexpr(" ",reference_genome.NC_numbers_species)
  reference_genome.NC_numbers <<- substring(reference_genome.NC_numbers_species,1,indices.blank-1)
  indices = which(reference_genome.NC_numbers == "")
  reference_genome.NC_numbers <<- reference_genome.NC_numbers [-indices]
  reference_genome.species <<- substring(reference_genome.NC_numbers_species,first=indices.blank+1)
  reference_genome.species <<- reference_genome.species [-indices]
  reference_genome.sequence_lengths <<- nchar(reference_genome_fasta[seq(2,length(reference_genome_fasta),by=2)])
  reference_genome.sequence_lengths <<- reference_genome.sequence_lengths [-indices]
  reference_genome.sequences <<- reference_genome_fasta[seq(2,length(reference_genome_fasta),by=2)]
  reference_genome.sequences <<- temp.sequences [-indices]
  reference_genome.NC_numbers_species <<- reference_genome.NC_numbers_species [-indices]
}

#' Simulation initialisation
#'
#' Before the actual simulation procedure can start, statistical distributions of the original mapping results must be calculated by the function \code{sim_init}.
#' The statistical distributions from the sam-files will be stored in the RDS objects directory, for each NGS run separately.
#'
#' Before using the function \code{sim_init}, the functions \code{\link{create_directories_and_file_paths}} and \code{\link{extract_information_from_reference_genome}} must be executed.
#'
#' @param file_indices Indices of the sam-files to be analysed
#' @param type_of_sam_file "init" for original sam-file or "sim" for sam-file of simulated fastq-files.
#'
#' @return None
#'
#' @examples
#' \dontrun{sim_init(file_indices=1,type_of_sam_file="init")} # Processing takes a few minutes of time!
#' \dontrun{sim_init(file_indices=1,type_of_sam_file="sim")} # Execute after mapping simulated fastq-files!
#'
#' @export
sim_init = function ( file_indices = rep(NA_integer_,0) , type_of_sam_file) {
  if ( length(file_indices) == 0 ) {
    file_indices = 1:length(sam.file_paths)
  }
  for ( i in file_indices ) { # For every sam file ...

    # Extract mapped read information from sam file and store as data frame:
    cat("Sim init iteration",i,"of",length(file_indices),"\n")
    {
      if ( type_of_sam_file == "init" ) {
        sam.file = readLines(sam.mapq_filtered.file_paths[i]) # Import init sam file.
      }
      else if ( type_of_sam_file == "sim" ) {
        sam.file = readLines(metasimulations_sam.mapq_filtered.file_paths[i]) # Import sim sam file.
      }
    }
    sam.file = strsplit(sam.file,"\t")
    sam.file = sapply(sam.file,FUN = function(x) x = x[1:11]) # Only first 11 columns are important.
    sam.file = t(sam.file)
    colnames(sam.file) = c("QNAME","FLAG","RNAME","POS","MAPQ","CIGAR","RNEXT","PNEXT","TLEN","SEQ","QUAL")
    sam.file = as.data.frame(sam.file)
    sam.file$QNAME = as.character(sam.file$QNAME)
    sam.file$FLAG = as.numeric(as.character(sam.file$FLAG))
    sam.file$POS = as.numeric(as.character(sam.file$POS))
    sam.file$MAPQ = as.numeric(as.character(sam.file$MAPQ))
    sam.file$CIGAR = as.character(sam.file$CIGAR)
    sam.file$RNEXT = as.character(sam.file$RNEXT)
    sam.file$PNEXT = as.character(sam.file$PNEXT)
    sam.file$TLEN = as.character(sam.file$TLEN)
    sam.file$SEQ = as.character(sam.file$SEQ)
    sam.file$QUAL = as.character(sam.file$QUAL)

    sam.file = sam.file [ complete.cases(sam.file[,c(3,4,5,9,10,11)]) , ] # Delete rows that contain at least one NA in specific columns.
    sam.file = sam.file [ which ( sam.file$RNAME != "*" ) , ] # Filter unmapped reads (should be done already).
    my.indices = match(sam.file$RNAME,reference_genome.NC_numbers)
    sam.file = sam.file [ which(!is.na(my.indices)) , ] # Filter reads not found in the reference genome.
    sam.file$RNAME = factor(sam.file$RNAME)

    my.order = order(sam.file$POS)
    sam.file = sam.file[my.order,]

    SEQ.str_splitted = strsplit(sam.file$SEQ,"")
    sam.file$read_lengths = sapply(sam.file$SEQ,nchar) # Read lengths
    QUAL.str_splitted = strsplit(sam.file$QUAL,"")

    # If read sequences contain other letters than "ACGT" (e.g. "N"), replace with sampled letters from "ACGT"-distribution:
    SEQ.table = SEQ.table.acgt = table((unlist(SEQ.str_splitted)))
    my.indices = is.element(names(SEQ.table),c("A","C","G","T"))
    which_symbols_not_acgt = names(SEQ.table)[my.indices == F]
    if ( length(which_symbols_not_acgt) > 0 ) {
      which_symbols_not_acgt = paste(which_symbols_not_acgt,collapse="|")
      SEQ.table.acgt = SEQ.table[my.indices]
      indices.N1 = sapply(SEQ.str_splitted,FUN=function(x) grep(which_symbols_not_acgt,x))
      indices.N1.lengths = sapply(indices.N1,length)
      indices.N1 = which(indices.N1.lengths > 0)
      indices.N2 = sapply(SEQ.str_splitted[indices.N1], FUN = function(x) grep(which_symbols_not_acgt,x))
      for ( j in 1:length(indices.N1) ) {
        my.random_bases = sample(c("A","C","G","T"),size=length(indices.N2[[j]]),prob = SEQ.table.acgt,replace = T)
        for ( k in 1:length(indices.N2[[j]]) ) {
          substring(sam.file$SEQ[indices.N1[j]],indices.N2[[j]] [k],indices.N2[[j]] [k]) = my.random_bases[k]
          SEQ.table.acgt[names(SEQ.table.acgt) == my.random_bases[k]] = SEQ.table.acgt[names(SEQ.table.acgt) == my.random_bases[k]] + 1
        }
        SEQ.str_splitted[[indices.N1[j]]] [indices.N2[[j]]] = my.random_bases
      }
    }

    attach(sam.file)
    total_number_of_reads = length(read_lengths)
    max.read_length = max(read_lengths)
    number_of_reads.mapped_to_species = sort(table(RNAME),decreasing = T)
    viruses_mapped.count = length(number_of_reads.mapped_to_species)
    MAPQ.table = table(MAPQ)
    indices.reads = POS_start.sorted = read_lengths.sorted = POS_end.sorted = vector("list",length=viruses_mapped.count)
    read_sequences.sorted = vector("list",length=viruses_mapped.count)
    QUAL.sorted = vector("list",length=viruses_mapped.count)
    indices.refs = rep(NA_integer_,viruses_mapped.count)
    reference_genome.sequence_lengths.sorted = rep(NA_integer_,viruses_mapped.count)
    reference_genome.sequences.sorted = rep(NA_character_,viruses_mapped.count)
    reference_genome.subsequences.sorted = vector("list",length=viruses_mapped.count)
    reference_genome.nucleotides = rep(NA_character_,0)
    names(reference_genome.sequence_lengths.sorted) = names(number_of_reads.mapped_to_species)
    names(reference_genome.sequences.sorted) = names(number_of_reads.mapped_to_species)
    names(reference_genome.subsequences.sorted) = names(number_of_reads.mapped_to_species)
    for ( j in 1:viruses_mapped.count ) {
      indices.reads[[j]] = grep(names(number_of_reads.mapped_to_species[j]),RNAME) # Indices of species in sam file
      indices.refs[j] = grep(names(number_of_reads.mapped_to_species[j]),reference_genome.NC_numbers) # Indices of mapped species in fasta file
      reference_genome.sequences.sorted[[j]] = reference_genome.sequences[indices.refs[j]] # Reference sequences sorted by counts mapped reads to virus
      reference_genome.subsequences.sorted[[j]] = vector("list",number_of_reads.mapped_to_species[j]) # Reference subsequences (start and end position from reads)
      reference_genome.sequence_lengths.sorted[j] = reference_genome.sequence_lengths[indices.refs[j]] # Reference sequence length of mapped species
      read_sequences.sorted[[j]] = vector("list",number_of_reads.mapped_to_species[j])
      QUAL.sorted[[j]] = vector("list",number_of_reads.mapped_to_species[j])
      POS_start.sorted[[j]] = POS[ indices.reads[[j]] ]
      read_lengths.sorted[[j]] = read_lengths[ indices.reads[[j]] ]
      POS_end.sorted[[j]] = POS_start.sorted[[j]] + read_lengths.sorted[[j]] - 1
      for ( k in 1:number_of_reads.mapped_to_species[j] ) {
        read_sequences.sorted[[j]] [[k]] = unlist(SEQ.str_splitted [ indices.reads[[j]] [k] ])
        QUAL.sorted[[j]] [[k]] = unlist(QUAL.str_splitted [ indices.reads[[j]] [k] ])
      }
    }
    reference_genome.sequences.sorted.str_splitted = strsplit(reference_genome.sequences.sorted,"")
    detach(sam.file)

    # Generate statistics of the differences between reads with and without errors:

    # Calculate error distributions based on position in reference genome.
    nucleotide.transition_frequencies = vector("list",viruses_mapped.count)
    names(nucleotide.transition_frequencies) = names(reference_genome.sequence_lengths.sorted)
    for ( j in 1:viruses_mapped.count ) {
      nucleotide.transition_frequencies [[j]] = matrix(0,nrow = 4, ncol = reference_genome.sequence_lengths.sorted[j])
      rownames(nucleotide.transition_frequencies [[j]]) = c("A","C","G","T")
      colnames(nucleotide.transition_frequencies [[j]]) = unlist(strsplit(reference_genome.sequences.sorted[[j]],""))
    }

    # Calculate quality value distributions based on position in read.
    quality_values.frequencies.equal.complete = character(0)
    quality_values.frequencies.not_equal.complete = character(0)
    quality_values.frequencies.equal = vector("list",max.read_length)
    quality_values.frequencies.not_equal = vector("list",max.read_length)

    indices.nucleotide.transition_frequencies.not_all_zero = vector("list",viruses_mapped.count)
    indices.nucleotide.transition_frequencies.all_zero = vector("list",viruses_mapped.count)
    for ( j in 1:viruses_mapped.count ) {
      cat("Virus",j,"of",viruses_mapped.count,"\n")
      # Calculate distributions of error and quality value.
      for ( k in 1:number_of_reads.mapped_to_species[j] ) {
        if ( k %% 100 == 0 ) print(round(k/number_of_reads.mapped_to_species[j]*100,1))
        my.start_pos = POS_start.sorted[[j]] [k]
        my.end_pos = POS_end.sorted[[j]] [k]
        indices.nucleotide.transition_frequencies.not_all_zero [[j]] = union(indices.nucleotide.transition_frequencies.not_all_zero[[j]],my.start_pos:my.end_pos)
        my.read_sequence = read_sequences.sorted[[j]] [[k]]
        my.ref_sequence = reference_genome.sequences.sorted.str_splitted[[j]] [my.start_pos:my.end_pos]
        reference_genome.subsequences.sorted[[j]] [[k]] = my.ref_sequence
        reference_genome.nucleotides = c(reference_genome.nucleotides,my.ref_sequence)
        my.quality_sequence = QUAL.sorted[[j]] [[k]]
        indices.equal = which(my.read_sequence == my.ref_sequence)
        indices.not_equal = which(my.read_sequence != my.ref_sequence)
        if ( length(indices.equal) > 0 ) {
          for ( l in indices.equal ) {
            quality_values.frequencies.equal.complete = c(quality_values.frequencies.equal.complete,my.quality_sequence[l])
            quality_values.frequencies.equal [[l]] = c(quality_values.frequencies.equal [[l]],my.quality_sequence[l])
          }
        }
        if ( length(indices.not_equal) > 0 ) {
          for ( l in indices.not_equal ) {
            quality_values.frequencies.not_equal.complete = c(quality_values.frequencies.not_equal.complete,my.quality_sequence[l])
            quality_values.frequencies.not_equal [[l]] = c(quality_values.frequencies.not_equal [[l]],my.quality_sequence[l])
          }
        }
        for ( l in my.start_pos:min(my.end_pos,reference_genome.sequence_lengths.sorted[j]) ) {
          if ( my.read_sequence[l-my.start_pos+1] == "A" ) {
            nucleotide.transition_frequencies [[j]] [1,l] = nucleotide.transition_frequencies [[j]] [1,l] + 1
          }
          else if ( my.read_sequence[l-my.start_pos+1] == "C" ) {
            nucleotide.transition_frequencies [[j]] [2,l] = nucleotide.transition_frequencies [[j]] [2,l] + 1
          }
          else if ( my.read_sequence[l-my.start_pos+1] == "G" ) {
            nucleotide.transition_frequencies [[j]] [3,l] = nucleotide.transition_frequencies [[j]] [3,l] + 1
          }
          else if ( my.read_sequence[l-my.start_pos+1] == "T" ) {
            nucleotide.transition_frequencies [[j]] [4,l] = nucleotide.transition_frequencies [[j]] [4,l] + 1
          }
        }
      }
      indices.nucleotide.transition_frequencies.all_zero [[j]] = setdiff(1:reference_genome.sequence_lengths.sorted[j],indices.nucleotide.transition_frequencies.not_all_zero [[j]])
    }
    REF.table.acgt = sort(table(reference_genome.nucleotides))
    REF.table.acgt = REF.table.acgt[order(names(REF.table.acgt))]
    REF.table.acgt = REF.table.acgt[names(REF.table.acgt) %in% c("A","C","G","T")]
    quality_values.frequencies.equal.complete = sort(table(quality_values.frequencies.equal.complete), decreasing = T)
    quality_values.frequencies.not_equal.complete = sort(table(quality_values.frequencies.not_equal.complete), decreasing = T)
    for ( j in 1:max.read_length ) {
      quality_values.frequencies.equal [[j]] = sort(table(quality_values.frequencies.equal [[j]]),decreasing = T)
      quality_values.frequencies.not_equal [[j]] = sort(table(quality_values.frequencies.not_equal [[j]]),decreasing = T)
    }

    # Save RDS objects:

    {
      if ( type_of_sam_file == "init" ) {
        metasimulations.rds_objects.directories = metasimulations.rds_objects.sim_init.directories
        my.chars = ""
      }
      else if ( type_of_sam_file == "sim" ) {
        metasimulations.rds_objects.directories = metasimulations.rds_objects.sim_sam.directories
        my.chars = ".sim"
      }
    }

    saveRDS(sam.file,paste0(metasimulations.rds_objects.directories[i],"/sam.file",my.chars,".rds"))

    saveRDS(REF.table.acgt,paste0(metasimulations.rds_objects.directories[i],"/REF.table.acgt",my.chars,".rds"))
    saveRDS(SEQ.table,paste0(metasimulations.rds_objects.directories[i],"/SEQ.table",my.chars,".rds"))
    saveRDS(SEQ.table.acgt,paste0(metasimulations.rds_objects.directories[i],"/SEQ.table.acgt",my.chars,".rds"))
    saveRDS(MAPQ.table,paste0(metasimulations.rds_objects.directories[i],"/MAPQ.table",my.chars,".rds"))
    saveRDS(max.read_length,paste0(metasimulations.rds_objects.directories[i],"/max.read_length",my.chars,".rds"))
    saveRDS(total_number_of_reads,paste0(metasimulations.rds_objects.directories[i],"/total_number_of_reads",my.chars,".rds"))
    saveRDS(number_of_reads.mapped_to_species,paste0(metasimulations.rds_objects.directories[i],"/number_of_reads.mapped_to_species",my.chars,".rds"))
    saveRDS(viruses_mapped.count,paste0(metasimulations.rds_objects.directories[i],"/viruses_mapped.count",my.chars,".rds"))

    saveRDS(reference_genome.sequences.sorted,paste0(metasimulations.rds_objects.directories[i],"/reference_genome.sequences.sorted",my.chars,".rds"))
    saveRDS(reference_genome.subsequences.sorted,paste0(metasimulations.rds_objects.directories[i],"/reference_genome.subsequences.sorted",my.chars,".rds"))
    saveRDS(reference_genome.sequence_lengths.sorted,paste0(metasimulations.rds_objects.directories[i],"/reference_genome.sequence_lengths.sorted",my.chars,".rds"))
    saveRDS(read_sequences.sorted,paste0(metasimulations.rds_objects.directories[i],"/read_sequences.sorted",my.chars,".rds"))
    saveRDS(POS_start.sorted,paste0(metasimulations.rds_objects.directories[i],"/POS_start.sorted",my.chars,".rds"))
    saveRDS(read_lengths.sorted,paste0(metasimulations.rds_objects.directories[i],"/read_lengths.sorted",my.chars,".rds"))
    saveRDS(POS_end.sorted,paste0(metasimulations.rds_objects.directories[i],"/POS_end.sorted",my.chars,".rds"))

    saveRDS(nucleotide.transition_frequencies,paste0(metasimulations.rds_objects.directories[i],"/nucleotide.transition_frequencies",my.chars,".rds"))
    saveRDS(indices.nucleotide.transition_frequencies.not_all_zero,paste0(metasimulations.rds_objects.directories[i],"/indices.nucleotide.transition_frequencies.not_all_zero",my.chars,".rds"))
    saveRDS(indices.nucleotide.transition_frequencies.all_zero,paste0(metasimulations.rds_objects.directories[i],"/indices.nucleotide.transition_frequencies.all_zero",my.chars,".rds"))
    saveRDS(quality_values.frequencies.equal.complete,paste0(metasimulations.rds_objects.directories[i],"/quality_values.frequencies.equal.complete",my.chars,".rds"))
    saveRDS(quality_values.frequencies.equal,paste0(metasimulations.rds_objects.directories[i],"/quality_values.frequencies.equal",my.chars,".rds"))
    saveRDS(quality_values.frequencies.not_equal.complete,paste0(metasimulations.rds_objects.directories[i],"/quality_values.frequencies.not_equal.complete",my.chars,".rds"))
    saveRDS(quality_values.frequencies.not_equal,paste0(metasimulations.rds_objects.directories[i],"/quality_values.frequencies.not_equal",my.chars,".rds"))

  }
}

#' Graphics of simulation initialisation
#'
#' This function produces graphics based on the statistical distributions of the original sam-files which are generated by the function \code{\link{sim_init}}.
#' The function plots distributions of nucleotide, mapping quality, read length, start position and quality value of all mapped viruses together and top three mapped viruses.
#' and writes the plots into one pdf-file.
#'
#' Before using the function \code{sim_init_graphics}, the functions \code{\link{create_directories_and_file_paths}}, \code{\link{extract_information_from_reference_genome}} and \code{\link{sim_init}} must be executed.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 qplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 scale_x_log10
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggsci scale_fill_npg
#' @importFrom ggpubr ggarrange
#' @importFrom seqTools char2ascii
#'
#' @param file_indices Indices of the sam-files to produce graphics of
#'
#' @return None
#'
#' @examples
#' sim_init_graphics ( file_indices = 1 )
#' @export
sim_init_graphics = function ( file_indices = rep(NA_integer_,0) ) {
  if ( length(file_indices) == 0 ) {
    file_indices = 1:length(sam.file_paths)
  }
  # Generate statistics graphics about the mapped reads of the original sam files:
  for ( i in file_indices ) { # For every sam file ...

    # Read RDS objects:
    my.RDS_objects.file_names = list.files(path=(metasimulations.rds_objects.sim_init.directories[i]))
    my.RDS_objects.file_names.short = substring(my.RDS_objects.file_names,1,nchar(my.RDS_objects.file_names)-4)
    my.RDS_objects.file_paths = paste0(metasimulations.rds_objects.sim_init.directories[i],my.RDS_objects.file_names)
    for ( j in 1:length(my.RDS_objects.file_paths) ) {
      assign(my.RDS_objects.file_names.short[j],readRDS(my.RDS_objects.file_paths[j]))
    }
    REF.table.acgt = REF.table.acgt[names(REF.table.acgt) %in% c("A","C","G","T")] # Delete later

    quality_values.identical.phred.old = quality_values.frequencies.equal
    quality_values.identical.phred.new = matrix(NA_integer_,nrow=3,ncol=max.read_length)
    for ( j in 1:max.read_length ) {
      names(quality_values.identical.phred.old[[j]]) = sapply(names(quality_values.identical.phred.old[[j]]), FUN = function(x) char2ascii(x) - 33)
      my.vector = rep(names(quality_values.identical.phred.old[[j]]), quality_values.identical.phred.old[[j]])
      my.vector = as.numeric(my.vector)
      quality_values.identical.phred.new[,j] = quantile(my.vector, probs = c(0.1,0.5,0.9))
    }
    rownames(quality_values.identical.phred.new) = c("10%","50%","90%")
    colnames(quality_values.identical.phred.new) = 1:max.read_length

    quality_values.not_identical.phred.old = quality_values.frequencies.not_equal
    quality_values.not_identical.phred.new = matrix(NA_integer_,nrow=3,ncol=max.read_length)
    for ( j in 1:max.read_length ) {
      names(quality_values.not_identical.phred.old[[j]]) = sapply(names(quality_values.not_identical.phred.old[[j]]), FUN = function(x) char2ascii(x) - 33)
      my.vector = rep(names(quality_values.not_identical.phred.old[[j]]), quality_values.not_identical.phred.old[[j]])
      my.vector = as.numeric(my.vector)
      quality_values.not_identical.phred.new[,j] = quantile(my.vector, probs = c(0.1,0.5,0.9))
    }
    rownames(quality_values.not_identical.phred.new) = c("10%","50%","90%")
    colnames(quality_values.not_identical.phred.new) = 1:max.read_length

    tihoblue = rgb(0, 106, 179, maxColorValue=255)

    pdf(metasimulations.graphics_sim_init.file_paths[i], onefile = T)

    qplot(number_of_reads.mapped_to_species,geom="density",col=I(tihoblue),xlab="Reads mapped to viruses",ylab="Density")

    nucleotide_frequency.table = data.frame(Sam_File = c(rep("Reference",4),rep("Read",4)),Nucleotide = rep(c("A","C","G","T"),2), Frequency = c(as.numeric(REF.table.acgt),as.numeric(SEQ.table.acgt)) )
    nucleotide_frequency.table$Sam_File = factor(nucleotide_frequency.table$Sam_File,levels=c("Reference","Read"))
    graphics.count = min(3,viruses_mapped.count)
    my.plots = vector("list",graphics.count)
    my.plots[[1]] = ggplot(data=nucleotide_frequency.table, aes(x=Nucleotide,y=Frequency)) + geom_bar(stat="identity", aes(fill = Sam_File), position = "dodge")+ scale_fill_npg() + xlab("Nucleotide (all mapped viruses)") + ylab("Frequency")
    for ( j in 1:graphics.count ) {
      my.sequences = read_sequences.sorted[[j]]
      my.SEQ.table.acgt = table(unlist(my.sequences))
      my.sequences = reference_genome.subsequences.sorted[[j]]
      my.REF.table.acgt = table(unlist(my.sequences))
      my.REF.table.acgt = my.REF.table.acgt[names(my.REF.table.acgt) %in% c("A","C","G","T")] # Delete later
      nucleotide_frequency.table = data.frame(Sam_File = c(rep("Reference",4),rep("Read",4)),Nucleotide = rep(c("A","C","G","T"),2), Frequency = c(as.numeric(my.REF.table.acgt),as.numeric(my.SEQ.table.acgt)) )
      nucleotide_frequency.table$Sam_File = factor(nucleotide_frequency.table$Sam_File,levels=c("Reference","Read"))
      my.plots[[j+1]] = ggplot(data=nucleotide_frequency.table, aes(x=Nucleotide,y=Frequency)) + geom_bar(stat="identity", aes(fill = Sam_File), position = "dodge") + scale_fill_npg() + xlab(paste0("Nucleotide (",names(number_of_reads.mapped_to_species)[j],")")) + ylab("Frequency")
    }
    print(ggarrange(plotlist = my.plots,common.legend = T, legend = "bottom", labels = "AUTO") )

    my.MAPQ.table = data.frame(Mapping.Quality = names(MAPQ.table), Proportion = prop.table(as.numeric(MAPQ.table))*100)
    my.MAPQ.table$Mapping.Quality = factor(my.MAPQ.table$Mapping.Quality,levels=sort(as.numeric(levels(my.MAPQ.table$Mapping.Quality))))
    my.plots = vector("list",graphics.count)
    my.plots[[1]] = ggplot(data=my.MAPQ.table, aes(x=Mapping.Quality,y=Proportion)) + geom_bar(stat="identity",fill = I(tihoblue), colour = "black") + xlab("Mapping Quality (all mapped viruses)") + ylab("Proportion (%)") + theme(axis.text.x = element_text(angle = 90,vjust = 0.5))
    for ( j in 1:graphics.count ) {
      my.MAPQ.values = subset(sam.file, RNAME == names(number_of_reads.mapped_to_species[j]), select = MAPQ)
      my.MAPQ.table = sort(table(my.MAPQ.values))
      my.MAPQ.table = data.frame(Mapping.Quality = names(my.MAPQ.table), Proportion = prop.table(as.numeric(my.MAPQ.table))*100)
      my.MAPQ.table$Mapping.Quality = factor(my.MAPQ.table$Mapping.Quality,levels=sort(as.numeric(levels(my.MAPQ.table$Mapping.Quality))))
      my.plots[[j+1]] = ggplot(data=my.MAPQ.table, aes(x=Mapping.Quality,y=Proportion)) + geom_bar(stat="identity",fill = I(tihoblue), colour = "black") + xlab(paste0("Mapping Quality (",names(number_of_reads.mapped_to_species)[j],")")) + ylab("Proportion (%)") + theme(axis.text.x = element_text(angle = 90,vjust = 0.5))
    }
    print(ggarrange(plotlist = my.plots, labels = "AUTO"))

    my.plots = vector("list",graphics.count)
    my.data_frame = data.frame(read_lengths = sam.file$read_lengths)
    my.plots[[1]] = ggplot(my.data_frame,aes(x=read_lengths)) + geom_histogram(breaks = seq(0,max.read_length,length=31), fill = I(tihoblue), col = I("black")) + xlab("Read Length (all mapped viruses)") + ylab("Count")
    for ( j in 1:graphics.count ) {
      my.data_frame = data.frame(read_lengths = read_lengths.sorted[[j]])
      my.plots[[j+1]] = ggplot(my.data_frame,aes(x=read_lengths)) + geom_histogram(breaks = seq(0,max.read_length,length=31), fill = I(tihoblue), col = I("black")) + xlab(paste0("Read Length (",names(number_of_reads.mapped_to_species)[j],")")) + ylab("Count") + xlim(-3,max.read_length+2)
    }
    print(ggarrange(plotlist = my.plots, labels = "AUTO") )

    my.plots = vector("list",graphics.count)
    my.data_frame = data.frame(start_positions = sam.file$POS)
    my.plots[[1]] = ggplot(my.data_frame,aes(x=start_positions)) + geom_density(col = I(tihoblue)) + scale_x_log10(limits = c(1,max(reference_genome.sequence_lengths.sorted))) + xlab("Start Positions (all mapped viruses)") + ylab("Density")
    for ( j in 1:graphics.count ) {
      my.data_frame = data.frame(start_positions = POS_start.sorted[[j]])
      my.plots[[j+1]] = ggplot(my.data_frame,aes(x=start_positions)) + geom_histogram(breaks=seq(0,reference_genome.sequence_lengths.sorted[j],length=31), fill = I(tihoblue), col = I("black")) + xlab(paste0("Start Positions (",names(number_of_reads.mapped_to_species)[j],")")) + ylab("Count")
    }
    print(ggarrange(plotlist = my.plots, labels = "AUTO") )

    my.data_frame = data.frame(Quantile_Values = c(quality_values.identical.phred.new[1,],quality_values.identical.phred.new[2,],quality_values.identical.phred.new[3,],
                                                   quality_values.not_identical.phred.new[1,],quality_values.not_identical.phred.new[2,],quality_values.not_identical.phred.new[3,]),
                               Quantiles = c(rep("id., 10%",max.read_length),rep("id., 50%",max.read_length),rep("id., 90%",max.read_length),
                                             rep("not id., 10%",max.read_length),rep("not id., 50%",max.read_length),rep("not id., 90%",max.read_length) ),
                               Position = rep(1:max.read_length,6))
    my.data_frame$Quantiles = factor(my.data_frame$Quantiles,levels=unique(my.data_frame$Quantiles))
    print(ggplot(my.data_frame, aes(x=Position, y=Quantile_Values, colour=Quantiles, size=Quantiles)) + xlab("Sequence position") + ylab("Phred score") +
            geom_line(aes(linetype=Quantiles)) + ylim(0,NA) +
            scale_colour_manual(values=c("#E64B35FF","#E64B35FF","#E64B35FF","#4DBBD5FF","#4DBBD5FF","#4DBBD5FF")) + ylim(0,NA) + scale_linetype_manual(values=c("dotted", "solid","dotted", "dotted","solid","dotted")) +
            scale_size_manual( values = c(0.4,0.8,0.4,0.4,0.8,0.4) ))

    dev.off()

  }
}

#' Simulation of fastq-files
#'
#' This function produces paired fastq-files based on the statistical distributions of the original sam-files which are generated by the function \code{\link{sim_init}}.
#' The simulated paired fastq-files will be stored in the fastq-directory.
#'
#' Do not use read_counts = "density" in combination with start_positions_and_read_lengths = "original"!
#' Before using the function \code{sim_fastq}, the functions \code{\link{create_directories_and_file_paths}}, \code{\link{extract_information_from_reference_genome}} and \code{\link{sim_init}} must be executed.
#'
#' @param file_indices Indices of the sam-files from which the rds-objects are used to simulate the paired fastq-files.
#' @param read_counts If the number of reads are "original", the number of reads generated per virus is identical to the read counts of the sam-file.
#' By specifying the parameter as "density", there is the possibility to change the number of reads generated randomly based on a kernel density estimation of the original number of reads mapped to the species.
#' In the latter case, the number of reads of the mapped species which are to be generated differs slightly from the original distribution.
#' @param start_positions_and_read_lengths If the start positions and read lengths are "original", the start positions and read lengths are taken from the original sam-files so that every read is used exactly once.
#' In contrary, if the parameter setting is "random", the reads are sampled with replacement so that some reads may be used multiple or zero times.
#' @param seed Seed of the random number generator used to create random base nucleotides and quality values based on the provided statistical distributions.
#'
#' @return None
#'
#' @examples
#' sim_fastq ( file_indices = 1 , read_counts = "density", start_positions_and_read_lengths = "random", seed = 42 ) # Takes a few seconds to run!
#' @export
sim_fastq = function ( file_indices = rep(NA_integer_,0) , read_counts , start_positions_and_read_lengths , seed ) {
  set.seed(seed)
  if ( length(file_indices) == 0 ) {
    file_indices = 1:length(sam.file_paths)
  }
  for ( i in file_indices ) { # For every sam file ...
    cat("Sim fastq iteration",i,"of",length(sam.file_paths),"\n")
    # Read RDS objects:
    my.RDS_objects.file_names = list.files(path=metasimulations.rds_objects.sim_init.directories[i])
    my.RDS_objects.file_names.short = substring(my.RDS_objects.file_names,1,nchar(my.RDS_objects.file_names)-4)
    my.RDS_objects.file_paths = paste0(metasimulations.rds_objects.sim_init.directories[i],my.RDS_objects.file_names)
    for ( j in 1:length(my.RDS_objects.file_paths) ) {
      assign(my.RDS_objects.file_names.short[j],readRDS(my.RDS_objects.file_paths[j]))
    }

    sam.file.all_mapq = readLines(sam.mapq_filtered.file_paths[i]) # Import sam file.
    sam.file.all_mapq = strsplit(sam.file.all_mapq,"\t")
    sam.file.all_mapq = sapply(sam.file.all_mapq,FUN = function(x) x[1:11]) # Only first 11 columns are important.
    sam.file.all_mapq = t(sam.file.all_mapq)

    colnames(sam.file.all_mapq) = c("QNAME","FLAG","RNAME","POS","MAPQ","CIGAR","RNEXT","PNEXT","TLEN","SEQ","QUAL")
    sam.file.all_mapq = as.data.frame(sam.file.all_mapq)
    sam.file.all_mapq$QNAME = as.character(sam.file.all_mapq$QNAME)
    sam.file.all_mapq$FLAG = as.numeric(as.character(sam.file.all_mapq$FLAG))
    sam.file.all_mapq$RNAME = as.factor(sam.file.all_mapq$RNAME)
    sam.file.all_mapq$POS = as.numeric(as.character(sam.file.all_mapq$POS))
    sam.file.all_mapq$MAPQ = as.numeric(as.character(sam.file.all_mapq$MAPQ))
    sam.file.all_mapq$CIGAR = as.character(sam.file.all_mapq$CIGAR)
    sam.file.all_mapq$RNEXT = as.character(sam.file.all_mapq$RNEXT)
    sam.file.all_mapq$PNEXT = as.character(sam.file.all_mapq$PNEXT)
    sam.file.all_mapq$TLEN = as.character(sam.file.all_mapq$TLEN)
    sam.file.all_mapq$SEQ = as.character(sam.file.all_mapq$SEQ)
    sam.file.all_mapq$QUAL = as.character(sam.file.all_mapq$QUAL)

    sam.file.all_mapq = sam.file.all_mapq [ complete.cases(sam.file.all_mapq[,c(3,4,5,9,10,11)]) , ] # Delete rows that contain at least one NA in specific columns.
    sam.file.all_mapq = sam.file.all_mapq [ which ( sam.file.all_mapq$RNAME != "*" ) , ] # Filter unmapped reads (should be done already).
    my.indices = match(sam.file.all_mapq$RNAME,reference_genome.NC_numbers)
    sam.file.all_mapq = sam.file.all_mapq [ which(!is.na(my.indices)) , ] # Filter reads not found in the reference genome.
    sam.file.all_mapq$RNAME = factor(sam.file.all_mapq$RNAME) # Drop unused levels.

    sam.file = readLines(sam.mapq_filtered.file_paths[i])
    sam.file = strsplit(sam.file,"\t")
    sam.file = sapply(sam.file,FUN = function(x) x[1:11]) # Only first 11 columns are important.
    sam.file = t(sam.file)

    colnames(sam.file) = c("QNAME","FLAG","RNAME","POS","MAPQ","CIGAR","RNEXT","PNEXT","TLEN","SEQ","QUAL")
    sam.file = as.data.frame(sam.file)
    sam.file$QNAME = as.character(sam.file$QNAME)
    sam.file$FLAG = as.numeric(as.character(sam.file$FLAG))
    sam.file$RNAME = as.factor(sam.file$RNAME)
    sam.file$POS = as.numeric(as.character(sam.file$POS))
    sam.file$MAPQ = as.numeric(as.character(sam.file$MAPQ))
    sam.file$CIGAR = as.character(sam.file$CIGAR)
    sam.file$RNEXT = as.character(sam.file$RNEXT)
    sam.file$PNEXT = as.character(sam.file$PNEXT)
    sam.file$TLEN = as.character(sam.file$TLEN)
    sam.file$SEQ = as.character(sam.file$SEQ)
    sam.file$QUAL = as.character(sam.file$QUAL)

    sam.file = sam.file [ complete.cases(sam.file[,c(3,4,5,9,10,11)]) , ] # Delete rows that contain at least one NA in specific columns.
    sam.file = sam.file [ which ( sam.file$RNAME != "*" ) , ] # Filter unmapped reads (should be done already).
    my.indices = match(sam.file$RNAME,reference_genome.NC_numbers)
    sam.file = sam.file [ which(!is.na(my.indices)) , ] # Filter reads not found in the reference genome.
    sam.file$RNAME = factor(sam.file$RNAME) # Drop unused levels.

    number_of_reads.mapped_to_species = sort(table(sam.file$RNAME),decreasing = T)
    RNAME.all_mapq = sam.file.all_mapq$RNAME
    number_of_reads.mapped_to_species.all_mapq = sort(table(RNAME.all_mapq),decreasing = T)
    my.indices = match(names(number_of_reads.mapped_to_species),names(number_of_reads.mapped_to_species.all_mapq))
    number_of_reads.mapped_to_species.all_mapq = number_of_reads.mapped_to_species.all_mapq[my.indices]
    viruses_mapped.count = length(number_of_reads.mapped_to_species)

    number_of_reads.belonging_to_species.new_fastq = number_of_reads.mapped_to_species.all_mapq # read_counts = "original"
    if ( read_counts == "density" ) { # read_counts = "density"
      dens = density(number_of_reads.mapped_to_species.all_mapq, from = 1)
      max.x = round(max(dens$x))
      dens = density(number_of_reads.mapped_to_species.all_mapq, from = 1, to = max.x, n = max.x)
      number_of_reads.belonging_to_species.new_fastq = sample.int(max.x, size = viruses_mapped.count, replace = T, prob = dens$y)
      number_of_reads.belonging_to_species.new_fastq = sort(number_of_reads.belonging_to_species.new_fastq, decreasing = T)
      my.rank = rank(number_of_reads.mapped_to_species.all_mapq, ties.method = "random")
      my.rank = length(my.rank) - my.rank + 1
      number_of_reads.belonging_to_species.new_fastq = number_of_reads.belonging_to_species.new_fastq[my.rank]
    }
    total_number_of_reads.new = sum(number_of_reads.belonging_to_species.new_fastq)
    saveRDS(number_of_reads.belonging_to_species.new_fastq,paste0(metasimulations.rds_objects.sim_init.directories[i],"/number_of_reads.belonging_to_species.new_fastq.rds"))

    # Sequence identifier:
    SEQ_ID = SEQ_ID.L = SEQ_ID.R = vector("list",length = viruses_mapped.count)
    for ( j in 1:viruses_mapped.count ) {
      SEQ_ID [[j]] = paste0("@",rep(names(number_of_reads.mapped_to_species)[j],number_of_reads.belonging_to_species.new_fastq[j]))
      SEQ_ID [[j]] = paste(SEQ_ID [[j]], 1:number_of_reads.belonging_to_species.new_fastq[j], sep=":")
      SEQ_ID.L [[j]] = paste(SEQ_ID [[j]],"L")
      SEQ_ID.R [[j]] = paste(SEQ_ID [[j]],"R")
    }

    # Raw sequences:
    raw_sequence_letters = indices.quality_values.equal = indices.quality_values.not_equal = vector("list",viruses_mapped.count)
    fastq.start_pos = fastq.end_pos = vector("list",viruses_mapped.count)
    for ( j in 1:viruses_mapped.count ) {
      raw_sequence_letters [[j]] = rep(NA_character_,number_of_reads.belonging_to_species.new_fastq[j])
      fastq.start_pos [[j]] = fastq.end_pos [[j]] = rep(NA_integer_,number_of_reads.belonging_to_species.new_fastq[j])
      indices.quality_values.equal [[j]] = vector("list",number_of_reads.belonging_to_species.new_fastq[j])
      indices.quality_values.not_equal [[j]] = vector("list",number_of_reads.belonging_to_species.new_fastq[j])
    }
    indices.read = lengths.read = vector("list",viruses_mapped.count)
    for ( j in 1:viruses_mapped.count ) {
      lengths.read [[j]] = rep(NA_integer_,number_of_reads.belonging_to_species.new_fastq[j])
    }
    raw_sequence_letters.left = raw_sequence_letters.right = raw_sequence_letters
    indices.quality_values.equal.left = indices.quality_values.equal.right = indices.quality_values.equal
    indices.quality_values.not_equal.left = indices.quality_values.not_equal.right = indices.quality_values.not_equal
    {
      if ( start_positions_and_read_lengths == "original" ) {
        for ( j in 1:viruses_mapped.count ) {
          indices.read [[j]] = 1:number_of_reads.belonging_to_species.new_fastq[j]
        }
      }
      else if ( start_positions_and_read_lengths == "random" ) {
        for ( j in 1:viruses_mapped.count ) {
          indices.read [[j]] = sample.int(number_of_reads.mapped_to_species[j],size = number_of_reads.belonging_to_species.new_fastq[j],replace = T)
          indices.read [[j]] = sort(indices.read [[j]])
        }
      }
    }
    for ( j in 1:viruses_mapped.count ) {
      ref_len = reference_genome.sequence_lengths.sorted [j]
      my.count = 0
      for ( k in indices.read[[j]] ) {
        my.count = my.count + 1
        my.ref_sequence = reference_genome.subsequences.sorted [[j]] [k] [[1]]
        my.read_sequence.left = my.read_sequence.right = rep(NA_character_,length(my.ref_sequence))
        lengths.read [[j]] [my.count] = length(my.ref_sequence)
        my.start_pos = fastq.start_pos [[j]] [my.count] = POS_start.sorted [[j]] [k]
        my.end_pos = fastq.end_pos [[j]] [my.count] = POS_end.sorted [[j]] [k]
        my.freq_table = nucleotide.transition_frequencies [[j]] [,my.start_pos:min(my.end_pos,ref_len)]
        for ( l in 1:ncol(my.freq_table) ) {
          my.read_sequence.left [l] = sample(c("A","C","G","T"), size = 1, prob = my.freq_table[,l])
          my.read_sequence.right [l] = sample(c("A","C","G","T"), size = 1, prob = my.freq_table[,l])
        }
        raw_sequence_letters.left [[j]] [my.count] = paste(my.read_sequence.left,collapse="")
        raw_sequence_letters.right [[j]] [my.count] = paste(my.read_sequence.right,collapse="")
        indices.quality_values.equal.left [[j]] [[my.count]] = which(my.read_sequence.left == my.ref_sequence)
        indices.quality_values.equal.right [[j]] [[my.count]] = which(my.read_sequence.right == my.ref_sequence)
        indices.quality_values.not_equal.left [[j]] [[my.count]] = which(my.read_sequence.left != my.ref_sequence)
        indices.quality_values.not_equal.right [[j]] [[my.count]] = which(my.read_sequence.right != my.ref_sequence)
      }
    }
    saveRDS(fastq.start_pos,paste0(metasimulations.rds_objects.sim_init.directories[i],"fastq.start_pos.rds"))
    saveRDS(fastq.end_pos,paste0(metasimulations.rds_objects.sim_init.directories[i],"fastq.end_pos.rds"))

    # Use quality value distributions for the two conditions "identical" and "not identical" (per read base position) to produce quality values:
    quality_values = vector("list",viruses_mapped.count)
    for ( j in 1:viruses_mapped.count ) {
      quality_values [[j]] = rep(NA_character_,number_of_reads.belonging_to_species.new_fastq[j])
    }
    quality_values.left = quality_values.right = quality_values
    for ( j in 1:viruses_mapped.count ) {
      for ( k in 1:number_of_reads.belonging_to_species.new_fastq[j] ) {
        my.qual_sequence.left = my.qual_sequence.right = rep(NA_character_,lengths.read [[j]] [k])
        if ( length(indices.quality_values.equal.left [[j]] [[k]]) > 0 ) {
          for ( l in indices.quality_values.equal.left [[j]] [[k]] ) {
            if ( length(quality_values.frequencies.equal [[l]]) > 0 ) {
              my.qual_sequence.left [l] = sample(names(quality_values.frequencies.equal [[l]]), size = 1, prob = quality_values.frequencies.equal [[l]])
            }
            else {
              my.qual_sequence.left [l] = sample(names(quality_values.frequencies.equal.complete), size = 1, prob = quality_values.frequencies.equal.complete)
            }
          }
        }
        if ( length(indices.quality_values.not_equal.left [[j]] [[k]]) > 0 ) {
          for ( l in indices.quality_values.not_equal.left [[j]] [[k]] ) {
            if ( length(quality_values.frequencies.not_equal [[l]]) > 0 ) {
              my.qual_sequence.left [l] = sample(names(quality_values.frequencies.not_equal [[l]]), size = 1, prob = quality_values.frequencies.not_equal [[l]])
            }
            else {
              my.qual_sequence.left [l] = sample(names(quality_values.frequencies.not_equal.complete), size = 1, prob = quality_values.frequencies.not_equal.complete)
            }
          }
        }
        quality_values.left [[j]] [k] = paste(my.qual_sequence.left,collapse="")
        if ( length(indices.quality_values.equal.right [[j]] [[k]]) > 0 ) {
          for ( l in indices.quality_values.equal.right [[j]] [[k]] ) {
            if ( length(quality_values.frequencies.equal [[l]]) > 0 ) {
              my.qual_sequence.right [l] = sample(names(quality_values.frequencies.equal [[l]]), size = 1, prob = quality_values.frequencies.equal [[l]])
            }
            else {
              my.qual_sequence.right [l] = sample(names(quality_values.frequencies.equal.complete), size = 1, prob = quality_values.frequencies.equal.complete)
            }
          }
        }
        if ( length(indices.quality_values.not_equal.right [[j]] [[k]]) > 0 ) {
          for ( l in indices.quality_values.not_equal.right [[j]] [[k]] ) {
            if ( length(quality_values.frequencies.not_equal [[l]]) > 0 ) {
              my.qual_sequence.right [l] = sample(names(quality_values.frequencies.not_equal [[l]]), size = 1, prob = quality_values.frequencies.not_equal [[l]])
            }
            else {
              my.qual_sequence.right [l] = sample(names(quality_values.frequencies.not_equal.complete), size = 1, prob = quality_values.frequencies.not_equal.complete)
            }
          }
        }
        quality_values.right [[j]] [k] = paste(my.qual_sequence.right,collapse="")
      }
    }

    # Build paired fastq-files:
    fastq.file.L = fastq.file.R = rep(NA_character_,4*total_number_of_reads.new)
    fastq.file.L [seq(1,4*total_number_of_reads.new,by=4)] = unlist(SEQ_ID.L)
    fastq.file.R [seq(1,4*total_number_of_reads.new,by=4)] = unlist(SEQ_ID.R)
    fastq.file.L [seq(2,4*total_number_of_reads.new,by=4)] = unlist(raw_sequence_letters.left)
    fastq.file.R [seq(2,4*total_number_of_reads.new,by=4)] = unlist(raw_sequence_letters.right)
    fastq.file.L [seq(3,4*total_number_of_reads.new,by=4)] = "+"
    fastq.file.R [seq(3,4*total_number_of_reads.new,by=4)] = "+"
    fastq.file.L [seq(4,4*total_number_of_reads.new,by=4)] = unlist(quality_values.left)
    fastq.file.R [seq(4,4*total_number_of_reads.new,by=4)] = unlist(quality_values.right)

    writeLines(fastq.file.L,metasimulations.fastq_left.file_paths[i])
    writeLines(fastq.file.R,metasimulations.fastq_right.file_paths[i])

  }
}

#' Mapping of fastq-files
#'
#' The simulated fastq-files are used to be mapped against the artificial viral reference genome with bowtie2. After this step, one can filter out reads with a low mapping quality.
#' The new sam-file with filtered reads is stored separately in the same directory.
#'
#' @importFrom Rbowtie2 bowtie2
#' @importFrom rChoiceDialogs rchoose.dir
#'
#' @param file_indices Indices of the simulated fast-file pairs that will be mapped.
#' @param reference_genome_index_bowtie2.directory # Prefix (folder) of bowtie2 index files
#' @param mapq_filter_threshold # Reads with a mapping quality below this threshold will be deleted from the sam-files.
#' @param threads Number of threads that are used for mapping
#'
#' @return None
#'
#' @examples
#' library(rChoiceDialogs)
#' my.prefix = rchoose.dir() # Choose prefix (folder) of reference genome bowtie2 index files
#' my.prefix = gsub("\\\\","/",my.prefix)
#' my.prefix = paste0(my.prefix,"/")
#' mapping_bowtie2 ( file_indices = 1 , reference_genome_index_bowtie2.directory = my.prefix , mapq_filter_threshold = 2 , threads = 2 ) # Takes a few seconds to run!
#' @export
mapping_bowtie2 = function ( file_indices = rep(NA_integer_,0) , reference_genome_index_bowtie2.directory , mapq_filter_threshold = 0 , threads ) {
  if ( length(file_indices) == 0 ) {
    file_indices = 1:length(metasimulations.fastq_left.file_paths)
  }
  for ( i in file_indices ) {
    bowtie2 ( bt2Index = paste0(reference_genome_index_bowtie2.directory,"viral_genomic"),
              seq1 = metasimulations.fastq_left.file_paths[i], seq2 = metasimulations.fastq_right.file_paths[i], samOutput = metasimulations_sam.file_paths[i],
              overwrite = T,paste0("--no-unal --no-hd --threads ",threads) )
    if ( mapq_filter_threshold > 0 ) {
      # Filtering out bad mapping quality reads of the sam-file and storing results in shorter sam-file:
      sam.file = readLines(metasimulations_sam.file_paths[i]) # Import sam file.
      sam.file.temp = strsplit(sam.file,"\t")
      MAPQ = sapply(sam.file.temp,function(x) x[5]) # Int 	MAPping Quality
      MAPQ = as.numeric(MAPQ)
      indices = which(MAPQ >= mapq_filter_threshold)
      sam.file = sam.file[indices]
      shorter.sam.file_path = metasimulations_sam.file_paths[i]
      shorter.sam.file_path = substring(shorter.sam.file_path,1,nchar(shorter.sam.file_path)-4)
      shorter.sam.file_path = paste0(shorter.sam.file_path,"_mapq_filtered.sam")
      writeLines(sam.file,shorter.sam.file_path)
    }
  }
}

#' Graphics of simulated sam-files
#'
#' This function produces graphics based on the statistical distributions of the original sam-files and the sam-files produced by mapping of the simulated fastq-files
#' which are generated by the function \code{\link{sim_init}} with parameters "init" and "sim".
#' The function plots distributions of nucleotide, mapping quality, read length, start position and quality value of all mapped viruses together and top three mapped viruses
#' for both types of sam-files and writes the plots into one pdf-file.
#'
#' Before using the function \code{sim_init_graphics}, the functions \code{\link{create_directories_and_file_paths}}, \code{\link{extract_information_from_reference_genome}} and \code{\link{sim_init}} with parameters "init" and "sim" must be executed.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 qplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 scale_x_log10
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggplot2 labs
#' @importFrom ggsci scale_fill_npg
#' @importFrom ggsci scale_colour_npg
#' @importFrom ggpubr ggarrange
#' @importFrom seqTools char2ascii
#'
#' @param file_indices Indices of the sam-files to produce graphics of
#'
#' @return None
#'
#' @examples
#' sim_sam_graphics ( file_indices = 1 )
#' @export
sim_sam_graphics = function ( file_indices = rep(NA_integer_,0) ) {
  if ( length(file_indices) == 0 ) {
    file_indices = 1:length(sam.file_paths)
  }
  # Generate statistics graphics about the mapped reads of the simulated sam files:
  for ( i in file_indices ) { # For every sam file ...

    # Read RDS objects:
    my.RDS_objects.file_names = list.files(path=metasimulations.rds_objects.sim_init.directories[i])
    my.RDS_objects.file_names.short = substring(my.RDS_objects.file_names,1,nchar(my.RDS_objects.file_names)-4)
    my.RDS_objects.file_paths = paste0(metasimulations.rds_objects.sim_init.directories[i],my.RDS_objects.file_names)
    for ( j in 1:length(my.RDS_objects.file_paths) ) {
      assign(my.RDS_objects.file_names.short[j],readRDS(my.RDS_objects.file_paths[j]))
    }

    my.RDS_objects.file_names = list.files(path=metasimulations.rds_objects.sim_sam.directories[i])
    my.RDS_objects.file_names.short = substring(my.RDS_objects.file_names,1,nchar(my.RDS_objects.file_names)-4)
    my.RDS_objects.file_paths = paste0(metasimulations.rds_objects.sim_sam.directories[i],my.RDS_objects.file_names)
    for ( j in 1:length(my.RDS_objects.file_paths) ) {
      assign(my.RDS_objects.file_names.short[j],readRDS(my.RDS_objects.file_paths[j]))
    }

    tihoblue = rgb(0, 106, 179, maxColorValue=255)

    pdf(metasimulations.graphics_sim_sam.file_paths[i], onefile = T)

    my.data_frame = data.frame(Sam.File = c(rep("original",viruses_mapped.count),rep("simulated",viruses_mapped.count)),
                               Reads_mapped_to_viruses = c(number_of_reads.mapped_to_species,number_of_reads.belonging_to_species.new_fastq) )
    qplot(Reads_mapped_to_viruses,data = my.data_frame,geom="density",col=Sam.File,xlab="Reads mapped to viruses",ylab="Density") + scale_colour_npg() + labs(colour = "Sam-File") + xlim(c(0,NA))

    nucleotide_frequency.table = data.frame(Sam.File = c(rep("original",4),rep("simulated",4)),Nucleotide = rep(c("A","C","G","T"),2), Frequency = c(as.numeric(SEQ.table.acgt),as.numeric(SEQ.table.acgt.sim)) )
    nucleotide_frequency.table$Sam.File = factor(nucleotide_frequency.table$Sam.File,levels=c("original","simulated"))
    graphics.count = min(3,viruses_mapped.count)
    my.plots = vector("list",graphics.count)
    my.plots[[1]] = ggplot(data=nucleotide_frequency.table, aes(x=Nucleotide,y=Frequency)) + geom_bar(stat="identity", aes(fill = Sam.File), position = "dodge")+ scale_fill_npg() + xlab("Nucleotide (all mapped viruses)") + ylab("Frequency")
    for ( j in 1:graphics.count ) {
      my.sequences = read_sequences.sorted[[j]]
      my.SEQ.table.acgt = table(unlist(my.sequences))
      my.sequences = read_sequences.sorted.sim[[j]]
      my.SEQ.table.acgt.sim = table(unlist(my.sequences))
      nucleotide_frequency.table = data.frame(Sam.File = c(rep("original",4),rep("simulated",4)),Nucleotide = rep(c("A","C","G","T"),2), Frequency = c(as.numeric(my.SEQ.table.acgt),as.numeric(my.SEQ.table.acgt.sim)) )
      nucleotide_frequency.table$Sam.File = factor(nucleotide_frequency.table$Sam.File,levels=c("original","simulated"))
      my.plots[[j+1]] = ggplot(data=nucleotide_frequency.table, aes(x=Nucleotide,y=Frequency)) + geom_bar(stat="identity", aes(fill = Sam.File), position = "dodge") + scale_fill_npg() + xlab(paste0("Nucleotide (",names(number_of_reads.mapped_to_species)[j],")")) + ylab("Frequency")
    }
    print(ggarrange(plotlist = my.plots,common.legend = T, legend = "bottom", labels = "AUTO") )

    my.data_frame = data.frame(Sam.File = c(rep("original",total_number_of_reads),rep("simulated",total_number_of_reads.sim)),
                               Mapping_Quality = c(rep(names(MAPQ.table),MAPQ.table),rep(names(MAPQ.table.sim),MAPQ.table.sim) ) )
    my.data_frame$Sam.File = factor(my.data_frame$Sam.File,levels=c("original","simulated"))
    my.data_frame$Mapping_Quality = as.numeric(my.data_frame$Mapping_Quality)
    position.of.colon = regexpr(":",sam.file.sim$QNAME)
    QNAME.short = substring(sam.file.sim$QNAME,1,position.of.colon-1)
    max.mapping_quality = max(my.data_frame$Mapping_Quality)
    graphics.count = min(3,viruses_mapped.count)
    my.plots = vector("list",graphics.count)
    my.plots[[1]] = ggplot(data = my.data_frame, aes(x = Mapping_Quality, colour = Sam.File)) + geom_density() + scale_colour_npg() + xlab("Mapping Quality (all mapped viruses)") + ylab("Density") + xlim(c(0,max.mapping_quality))
    for ( j in 1:graphics.count ) {
      my.MAPQ = unlist(subset(sam.file,RNAME == names(number_of_reads.mapped_to_species[j]), select = MAPQ))
      my.MAPQ.sim = unlist(subset(sam.file.sim,RNAME == names(number_of_reads.mapped_to_species.sim[j]), select = MAPQ))
      my.data_frame = data.frame(Sam.File = c(rep("original",length(my.MAPQ)),rep("simulated",length(my.MAPQ.sim))),
                                 Mapping_Quality = c(my.MAPQ,my.MAPQ.sim ) )
      my.plots[[j+1]] = ggplot(data = my.data_frame, aes(x = Mapping_Quality, colour = Sam.File)) + geom_density() + scale_colour_npg() + xlab(paste0("Mapping Quality (",names(number_of_reads.mapped_to_species)[j],")")) + ylab("Density") + xlim(c(0,max.mapping_quality))
    }
    print(ggarrange(plotlist = my.plots,common.legend = T, legend = "bottom", labels = "AUTO") )

    my.data_frame = data.frame(Sam.File = c(rep("original",total_number_of_reads),rep("simulated",total_number_of_reads.sim)),
                               Read_Length = c(sam.file$read_lengths,sam.file.sim$read_lengths ) )
    my.data_frame$Sam.File = factor(my.data_frame$Sam.File,levels=c("original","simulated"))
    position.of.colon = regexpr(":",sam.file.sim$QNAME)
    QNAME.short = substring(sam.file.sim$QNAME,1,position.of.colon-1)
    graphics.count = min(3,viruses_mapped.count)
    my.plots = vector("list",graphics.count)
    my.plots[[1]] = ggplot(data = my.data_frame, aes(x = Read_Length, colour = Sam.File)) + geom_density() + scale_colour_npg() + xlab("Read length (all mapped viruses)") + ylab("Density") + xlim(c(0,max.read_length))
    for ( j in 1:graphics.count ) {
      my.read_length = unlist(subset(sam.file,RNAME == names(number_of_reads.mapped_to_species[j]), select = read_lengths))
      my.read_length.sim = unlist(subset(sam.file.sim,QNAME.short == names(number_of_reads.mapped_to_species.sim[j]), select = read_lengths))
      my.data_frame = data.frame(Sam.File = c(rep("original",length(my.read_length)),rep("simulated",length(my.read_length.sim))),
                                 Read_Length = c(my.read_length,my.read_length.sim ) )
      my.plots[[j+1]] = ggplot(data = my.data_frame, aes(x = Read_Length, colour = Sam.File)) + geom_density() + scale_colour_npg() + xlab(paste0("Read length (",names(number_of_reads.mapped_to_species)[j],")")) + ylab("Density") + xlim(c(0,max.read_length))
    }
    print(ggarrange(plotlist = my.plots,common.legend = T, legend = "bottom", labels = "AUTO") )



    my.data_frame = data.frame(Sam.File = c(rep("original",total_number_of_reads),rep("simulated",total_number_of_reads.sim)),
                               Start_Pos = c(sam.file$POS,sam.file.sim$POS ) )
    my.data_frame$Sam.File = factor(my.data_frame$Sam.File,levels=c("original","simulated"))
    position.of.colon = regexpr(":",sam.file.sim$QNAME)
    QNAME.short = substring(sam.file.sim$QNAME,1,position.of.colon-1)
    graphics.count = min(3,viruses_mapped.count)
    my.plots = vector("list",graphics.count)
    #my.plots[[1]] = ggplot(data = my.data_frame, aes(x = Start_Pos, colour = Sam.File)) + geom_density() + scale_colour_npg() + xlab("Start position (all mapped viruses)") + ylab("Density") + xlim(c(0,NA))
    for ( j in 1:graphics.count ) {
      my.start_pos = unlist(subset(sam.file,RNAME == names(number_of_reads.mapped_to_species[j]), select = POS))
      my.start_pos.sim = unlist(subset(sam.file.sim,QNAME.short == names(number_of_reads.mapped_to_species.sim[j]), select = POS))
      my.data_frame = data.frame(Sam.File = c(rep("original",length(my.start_pos)),rep("simulated",length(my.start_pos.sim))),
                                 Start_Pos = c(my.start_pos,my.start_pos.sim ) )
      my.plots[[j+1]] = ggplot(data = my.data_frame, aes(x = Start_Pos, colour = Sam.File)) + geom_density() + scale_colour_npg() + xlab(paste0("Start position (",names(number_of_reads.mapped_to_species)[j],")")) + ylab("Density") + xlim(c(0,NA))
    }
    print(ggarrange(plotlist = my.plots,common.legend = T, legend = "bottom", labels = "AUTO") )


    quality_values.identical.phred.old = quality_values.frequencies.equal
    quality_values.identical.phred.new = rep(NA_integer_,max.read_length)
    for ( j in 1:max.read_length ) {
      names(quality_values.identical.phred.old[[j]]) = sapply(names(quality_values.identical.phred.old[[j]]), FUN = function(x) char2ascii(x) - 33)
      my.vector = rep(names(quality_values.identical.phred.old[[j]]), quality_values.identical.phred.old[[j]])
      my.vector = as.numeric(my.vector)
      quality_values.identical.phred.new[j] = median(my.vector)
    }

    quality_values.not_identical.phred.old = quality_values.frequencies.not_equal
    quality_values.not_identical.phred.new = rep(NA_integer_,max.read_length)
    for ( j in 1:max.read_length ) {
      names(quality_values.not_identical.phred.old[[j]]) = sapply(names(quality_values.not_identical.phred.old[[j]]), FUN = function(x) char2ascii(x) - 33)
      my.vector = rep(names(quality_values.not_identical.phred.old[[j]]), quality_values.not_identical.phred.old[[j]])
      my.vector = as.numeric(my.vector)
      quality_values.not_identical.phred.new[j] = median(my.vector)
    }

    quality_values.identical.phred.old = quality_values.frequencies.equal.sim
    quality_values.identical.phred.new.sim = rep(NA_integer_,max.read_length)
    my.lengths = sapply(quality_values.identical.phred.old,length)
    for ( j in 1:max.read_length.sim ) {
      names(quality_values.identical.phred.old[[j]]) = sapply(names(quality_values.identical.phred.old[[j]]), FUN = function(x) char2ascii(x) - 33)
      my.vector = rep(names(quality_values.identical.phred.old[[j]]), quality_values.identical.phred.old[[j]])
      my.vector = as.numeric(my.vector)
      quality_values.identical.phred.new.sim[j] = median(my.vector)
    }

    quality_values.not_identical.phred.old = quality_values.frequencies.not_equal.sim
    quality_values.not_identical.phred.new.sim = rep(NA_integer_,max.read_length)
    for ( j in 1:max.read_length.sim ) {
      names(quality_values.not_identical.phred.old[[j]]) = sapply(names(quality_values.not_identical.phred.old[[j]]), FUN = function(x) char2ascii(x) - 33)
      my.vector = rep(names(quality_values.not_identical.phred.old[[j]]), quality_values.not_identical.phred.old[[j]])
      my.vector = as.numeric(my.vector)
      quality_values.not_identical.phred.new.sim[j] = median(my.vector)
    }

    my.lengths = numeric(4)
    my.lengths[1] = length(quality_values.identical.phred.new)
    my.lengths[2] = length(quality_values.not_identical.phred.new)
    my.lengths[3] = length(quality_values.identical.phred.new.sim)
    my.lengths[4] = length(quality_values.not_identical.phred.new.sim)
    my.data_frame = data.frame(Quantile_Values = c(quality_values.identical.phred.new[1:min(my.lengths)],quality_values.not_identical.phred.new[1:min(my.lengths)],quality_values.identical.phred.new.sim[1:min(my.lengths)],quality_values.not_identical.phred.new.sim[1:min(my.lengths)]),
                               Medians = c(rep("orig., ident.",max.read_length),rep("orig., not ident.",max.read_length),rep("sim., ident.",max.read_length),rep("sim., not ident.",max.read_length)),
                               Position = rep(1:max.read_length,4))
    my.data_frame$Medians = factor(my.data_frame$Medians,levels=unique(my.data_frame$Medians))
    print(ggplot(my.data_frame, aes(x=Position, y=Quantile_Values, colour=Medians, size=Medians)) + xlab("Sequence position") + ylab("Phred score") +
            geom_line(aes(linetype=Medians)) + ylim(0,NA) +
            scale_colour_manual(values=c("#E64B35FF","#E64B35FF","#4DBBD5FF","#4DBBD5FF")) + ylim(0,NA) + scale_linetype_manual(values=c("solid", "dotted","solid", "dotted")) +
            scale_size_manual( values = c(0.8,0.4,0.8,0.4) ))

    dev.off()

  }
}

#' Calculate consensus sequence of all original reads
#'
#' In order to visualise the simulation of read sequences, this function produces a four-coloured graphic
#' in which the colours blue, green, red and yellow represent the nucleotides A, C, G and T.
#' The read sequences of the original sam-file are plotted in rows whereupon each column corresponds to a base position of the reference genome.
#' Below the mapped read sequences, the consensus sequence is displayed and at the bottom the subsection of the reference genome belonging to the reads.
#' The graphic is stored in the temporary directory.
#'
#' Before using the function \code{consensus_sequence}, the functions \code{\link{create_directories_and_file_paths}} and \code{\link{extract_information_from_reference_genome}} must be executed.
#'
#' @param i Index of sam-file
#' @param j Index of mapped virus
#'
#' @return None
#'
#' @examples
#' consensus_sequence ( i = 1 , j = 3 )
#' @export
consensus_sequence = function ( i , j ) { # i: index of sam file; j: index of mapped virus
  read_sequences = readRDS(paste0(metasimulations.rds_objects.sim_init.directories[i],"read_sequences.sorted.rds"))
  read_sequences = read_sequences[[j]]
  start_pos = readRDS(paste0(metasimulations.rds_objects.sim_init.directories[i],"POS_start.sorted.rds"))
  start_pos = unlist(start_pos[j])
  end_pos = readRDS(paste0(metasimulations.rds_objects.sim_init.directories[i],"POS_end.sorted.rds"))
  end_pos = unlist(end_pos[j])
  ref_sequence = readRDS(paste0(metasimulations.rds_objects.sim_init.directories[i],"reference_genome.sequences.sorted.rds"))
  ref_sequence = ref_sequence[j]
  ref_sequence = substring(ref_sequence,min(start_pos),max(end_pos))
  ref_sequence = unname(unlist(strsplit(ref_sequence,"")))
  coverage.range = min(start_pos):max(end_pos)
  read_sequences.matrix = matrix(nrow = length(read_sequences)+16,ncol = diff(range(coverage.range))+1 )
  read_sequences.matrix [length(read_sequences)+16,] = ref_sequence
  for ( k in 1:length(read_sequences) ) {
    read_sequences.matrix [length(read_sequences)-k+1,(start_pos[k]-min(start_pos)+1):(end_pos[k]-min(start_pos)+1)] = read_sequences[[k]]
  }

  read_sequences.table = apply(read_sequences.matrix,MARGIN = 2,FUN = function(x) table(factor(x,levels=c("A","C","G","T"))))
  consensus_sequence = apply(read_sequences.table,MARGIN = 2,which.max)

  numeric.matrix = read_sequences.matrix
  my.indices = which(numeric.matrix == "A", arr.ind = T)
  numeric.matrix[my.indices] = 1
  my.indices = which(numeric.matrix == "C", arr.ind = T)
  numeric.matrix[my.indices] = 2
  my.indices = which(numeric.matrix == "G", arr.ind = T)
  numeric.matrix[my.indices] = 3
  my.indices = which(numeric.matrix == "T", arr.ind = T)
  numeric.matrix[my.indices] = 4
  numeric.matrix = apply(numeric.matrix,MARGIN = 2, FUN = function(y) as.numeric(y) )
  numeric.matrix[length(read_sequences)+11,] = consensus_sequence
  rotate = function(x) t(apply(x, 2, rev))

  my.file_path = paste0(metasimulations.temp.directory,"consensus_",i,"_",j,".pdf")
  pdf(my.file_path)
  image(rotate(numeric.matrix),col = c("blue","green","red","yellow"),xaxt = "n",yaxt = "n",xlab = "Base positions of reference genome",ylab = "Original reads (sorted by start position) and consensus sequence")
  dev.off()

}

#' Calculation of error rates
#'
#' In total, this function computes six different diagnostic parameters counting relative frequencies, two mapping rates and four error rates.
#' Furthermore, there are three absolute frequencies which count all mapped reads, reads that are mapped to an excel list of viruses and reads that are mapped correctly.
#' These values are calculated by using information of the mapping files of the simulated fastq-files from the previous step.
#' The old excel-file from the original mapping results is completed by six columns for the diagnostic parameters and saved as a new excel-file.
#'
#' @importFrom xlsx read.xlsx
#' @importFrom xlsx write.xlsx
#'
#' @param file_indices Indices of the mapped sam-files belonging to the simulated paired fastq-files
#'
#' @return None
#'
#' @examples
#' error_rates ( file_indices = 1 )
#' @export
error_rates = function ( file_indices = 1 ) {
  if ( length(file_indices) == 0 ) {
    file_indices = 1:length(metasimulations_sam.file_paths)
  }
  my.metasimulations_sam.file_paths = metasimulations_sam.file_paths [file_indices]
  my.metasimulations_sam.file_paths = substring(my.metasimulations_sam.file_paths,1,nchar(my.metasimulations_sam.file_paths)-4)
  my.metasimulations_sam.file_paths = paste0(my.metasimulations_sam.file_paths,"_mapq_filtered.sam")

  count.file = 0
  for ( i in file_indices ) { # For every sam file ...
    count.file = count.file + 1
    cat("Error rates file",count.file,"of",length(my.metasimulations_sam.file_paths),"\n")
    excel.file = read.xlsx(excel.file_paths[i],1,header=T)
    mapped_reads.NC_numbers = excel.file$Species_ID
    number_of_rows.excel_file = dim(excel.file)[1]
    excel.file.new = excel.file
    excel.file.new$SIM.count.new = readRDS(paste0(metasimulations.rds_objects.sim_init.directories[i],"number_of_reads.belonging_to_species.new_fastq.rds"))
    mapped_reads.read_count = excel.file.new$SIM.count.new
    excel.file.new$SIM.mapped = rep(NA_real_,number_of_rows.excel_file)
    excel.file.new$SIM.listed = rep(NA_real_,number_of_rows.excel_file)
    excel.file.new$SIM.correct = rep(NA_real_,number_of_rows.excel_file)
    excel.file.new$SIM.mapped_all.percent = rep(NA_real_,number_of_rows.excel_file)
    excel.file.new$SIM.listed_all.percent = rep(NA_real_,number_of_rows.excel_file)
    excel.file.new$SIM.correct_all.percent = rep(NA_real_,number_of_rows.excel_file)
    excel.file.new$SIM.listed_mapped.percent = rep(NA_real_,number_of_rows.excel_file)
    excel.file.new$SIM.correct_mapped.percent = rep(NA_real_,number_of_rows.excel_file)
    excel.file.new$SIM.correct_listed.percent = rep(NA_real_,number_of_rows.excel_file)

    # Extract mapped read information from sam file:

    cat("Iteration",i,"of",length(file_indices),"\n")
    sam.file = readLines(my.metasimulations_sam.file_paths[i]) # Import sam file.
    sam.file = strsplit(sam.file,"\t")
    sam.file = sapply(sam.file,FUN = function(x) x[1:11]) # Only first 11 columns are important.
    sam.file = t(sam.file)

    colnames(sam.file) = c("QNAME","FLAG","RNAME","POS","MAPQ","CIGAR","RNEXT","PNEXT","TLEN","SEQ","QUAL")
    sam.file = as.data.frame(sam.file)
    sam.file$QNAME = as.character(sam.file$QNAME)
    sam.file$FLAG = as.numeric(as.character(sam.file$FLAG))
    sam.file$RNAME = as.factor(sam.file$RNAME)
    sam.file$POS = as.numeric(as.character(sam.file$POS))
    sam.file$MAPQ = as.numeric(as.character(sam.file$MAPQ))
    sam.file$CIGAR = as.character(sam.file$CIGAR)
    sam.file$RNEXT = as.character(sam.file$RNEXT)
    sam.file$PNEXT = as.character(sam.file$PNEXT)
    sam.file$TLEN = as.character(sam.file$TLEN)
    sam.file$SEQ = as.character(sam.file$SEQ)
    sam.file$QUAL = as.character(sam.file$QUAL)

    sam.file = sam.file [ complete.cases(sam.file[,c(3,4,5,9,10,11)]) , ] # Delete rows that contain at least one NA in specific columns.
    sam.file = sam.file [ which ( sam.file$RNAME != "*" ) , ] # Filter unmapped reads (should be done already).
    my.indices = match(sam.file$RNAME,reference_genome.NC_numbers)
    sam.file = sam.file [ which(!is.na(my.indices)) , ] # Filter reads not found in the reference genome.
    sam.file$RNAME = factor(sam.file$RNAME) # Drop unused levels.

    QNAME = sam.file$QNAME
    position.of.colon = regexpr(":",QNAME)
    QNAME.short = substring(QNAME,1,position.of.colon-1)

    mapped_outside_of_list = vector("list", length = number_of_rows.excel_file)

    for ( j in 1:number_of_rows.excel_file ) {
      my.sam.file = subset(sam.file,QNAME.short == mapped_reads.NC_numbers[j])
      my.RNAME = my.sam.file$RNAME

      count.Omega = mapped_reads.read_count[j] * 2
      count.M = nrow(my.sam.file)
      count.M_list = sum( my.RNAME %in% as.character(mapped_reads.NC_numbers))
      my.indices = which ( my.RNAME %in% as.character(mapped_reads.NC_numbers) == FALSE )
      mapped_outside_of_list[[j]] = my.sam.file$RNAME[my.indices]
      count.C = sum(my.RNAME == as.character(mapped_reads.NC_numbers[j]))

      mapped_all.percent = round(count.M / count.Omega * 100,1)
      listed_all.percent = round(count.M_list / count.Omega * 100,1)
      correct_all.percent = round(count.C / count.Omega * 100,1)
      listed_mapped.percent = round(count.M_list / count.M * 100,1)
      correct_mapped.percent = round(count.C / count.M * 100,1)
      correct_listed.percent = round(count.C / count.M_list * 100,1)
      mapped = count.M
      listed = count.M_list
      correct = count.C

      excel.file.new$SIM.mapped_all.percent[j] = mapped_all.percent
      excel.file.new$SIM.listed_all.percent[j] = listed_all.percent
      excel.file.new$SIM.correct_all.percent[j] = correct_all.percent
      excel.file.new$SIM.listed_mapped.percent[j] = listed_mapped.percent
      excel.file.new$SIM.correct_mapped.percent[j] = correct_mapped.percent
      excel.file.new$SIM.correct_listed.percent[j] = correct_listed.percent
      excel.file.new$SIM.mapped[j] = mapped
      excel.file.new$SIM.listed[j] = listed
      excel.file.new$SIM.correct[j] = correct
    }
    new.excel.file_path = paste0(substring(excel.file_paths[i],1,nchar(excel.file_paths[i])-5),"_error_rates.xlsx")
    write.xlsx(excel.file.new,new.excel.file_path)

    mapped_outside_of_list = sort(table(droplevels(unlist(mapped_outside_of_list))),decreasing = T)
    my.indices = match(names(mapped_outside_of_list),reference_genome.NC_numbers)
    my.data_frame = data.frame(Species_ID = names(mapped_outside_of_list),
                               Species = reference_genome.species [my.indices], read_count = as.numeric(mapped_outside_of_list))
    new.excel.file_path = paste0(substring(excel.file_paths[i],1,nchar(excel.file_paths[i])-5),"_mapped_outside_of_list.xlsx")
    if ( nrow(my.data_frame) > 0 ) {
      write.xlsx(my.data_frame,new.excel.file_path)
    }
  }
}
