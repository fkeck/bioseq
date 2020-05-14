#' Genetic code tables
#'
#' List of all genetic code tables available in \code{bioseq}.
#' The number in bold can be used to select a table in appropriate functions.
#'
#' @section Available genetic codes:
#'
#' \strong{1}\code{.    }Standard
#'
#' \strong{2}\code{.    }Vertebrate Mitochondrial
#'
#' \strong{3}\code{.    }Yeast Mitochondrial
#'
#' \strong{4}\code{.    }Mold Mitochondrial; Protozoan Mitochondrial;
#' Coelenterate Mitochondrial; Mycoplasma; Spiroplasma
#'
#' \strong{5}\code{.    }Invertebrate Mitochondrial
#'
#' \strong{6}\code{.    }Ciliate Nuclear; Dasycladacean Nuclear;
#' Hexamita Nuclear
#'
#' \strong{9}\code{.    }Echinoderm Mitochondrial; Flatworm Mitochondrial
#'
#' \strong{10}\code{.    }Euplotid Nuclear
#'
#' \strong{11}\code{.    }Bacterial, Archaeal and Plant Plastid
#'
#' \strong{12}\code{.    }Alternative Yeast Nuclear
#'
#' \strong{13}\code{.    }Ascidian Mitochondrial
#'
#' \strong{14}\code{.    }Alternative Flatworm Mitochondrial
#'
#' \strong{15}\code{.    }Blepharisma Macronuclear
#'
#' \strong{16}\code{.    }Chlorophycean Mitochondrial
#'
#' \strong{21}\code{.    }Trematode Mitochondrial
#'
#' \strong{22}\code{.    }Scenedesmus obliquus Mitochondrial
#'
#' \strong{23}\code{.    }Thraustochytrium Mitochondrial
#'
#' \strong{24}\code{.    }Pterobranchia Mitochondrial
#'
#' \strong{25}\code{.    }Candidate Division SR1 and Gracilibacteria
#'
#' \strong{26}\code{.    }Pachysolen tannophilus Nuclear
#'
#' \strong{27}\code{.    }Karyorelict Nuclear
#'
#' \strong{28}\code{.    }Condylostoma Nuclear
#'
#' \strong{29}\code{.    }Mesodinium Nuclear
#'
#' \strong{30}\code{.    }Peritrich Nuclear
#'
#' \strong{31}\code{.    }Blastocrithidia Nuclear
#'
#' \strong{32}\code{.    }Balanophoraceae Plastid
#'
#' \strong{33}\code{.    }Cephalodiscidae Mitochondrial
#'
#' @references
#' Andrzej (Anjay) Elzanowski and Jim Ostell at
#' National Center for Biotechnology Information (NCBI),
#' Bethesda, Maryland, U.S.A.
#' \url{https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/index.cgi?chapter=tgencodes}
#'
#' @name genetic-codes
NULL


#' Genetic code tables
#'
#' The function returns a list of named vectors with
#' Start, Stop and Full_name attributes.
#'
#' @return A list of genetic code tables for DNA/RNA translation.
#'
dic_genetic_codes <- function() {
    list(`1` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("TTG", "CTG", "ATG"),
        Codon_stop = c("TAA", "TAG", "TGA"),
        Full_name = "Standard"),
    `2` = structure(
        c(TTT = "F",
          TTC = "F", TTA = "L", TTG = "L", TCT = "S", TCC = "S", TCA = "S",
          TCG = "S", TAT = "Y", TAC = "Y", TAA = "*", TAG = "*", TGT = "C",
          TGC = "C", TGA = "W", TGG = "W", CTT = "L", CTC = "L", CTA = "L",
          CTG = "L", CCT = "P", CCC = "P", CCA = "P", CCG = "P", CAT = "H",
          CAC = "H", CAA = "Q", CAG = "Q", CGT = "R", CGC = "R", CGA = "R",
          CGG = "R", ATT = "I", ATC = "I", ATA = "M", ATG = "M", ACT = "T",
          ACC = "T", ACA = "T", ACG = "T", AAT = "N", AAC = "N", AAA = "K",
          AAG = "K", AGT = "S", AGC = "S", AGA = "*", AGG = "*", GTT = "V",
          GTC = "V", GTA = "V", GTG = "V", GCT = "A", GCC = "A", GCA = "A",
          GCG = "A", GAT = "D", GAC = "D", GAA = "E", GAG = "E", GGT = "G",
          GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("ATT", "ATC", "ATA", "ATG", "GTG"),
        Codon_stop = c("TAA", "TAG", "AGA", "AGG"),
        Full_name = "Vertebrate Mitochondrial"),
    `3` = structure(
        c(TTT = "F",
          TTC = "F", TTA = "L", TTG = "L", TCT = "S", TCC = "S", TCA = "S",
          TCG = "S", TAT = "Y", TAC = "Y", TAA = "*", TAG = "*", TGT = "C",
          TGC = "C", TGA = "W", TGG = "W", CTT = "T", CTC = "T", CTA = "T",
          CTG = "T", CCT = "P", CCC = "P", CCA = "P", CCG = "P", CAT = "H",
          CAC = "H", CAA = "Q", CAG = "Q", CGT = "R", CGC = "R", CGA = "R",
          CGG = "R", ATT = "I", ATC = "I", ATA = "M", ATG = "M", ACT = "T",
          ACC = "T", ACA = "T", ACG = "T", AAT = "N", AAC = "N", AAA = "K",
          AAG = "K", AGT = "S", AGC = "S", AGA = "R", AGG = "R", GTT = "V",
          GTC = "V", GTA = "V", GTG = "V", GCT = "A", GCC = "A", GCA = "A",
          GCG = "A", GAT = "D", GAC = "D", GAA = "E", GAG = "E", GGT = "G",
          GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("ATA", "ATG", "GTG"),
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Yeast Mitochondrial"),
    `4` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "W", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("TTA", "TTG", "CTG", "ATT", "ATC", "ATA", "ATG", "GTG"),
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Mold Mitochondrial; Protozoan Mitochondrial; Coelenterate Mitochondrial; Mycoplasma; Spiroplasma"),
    `5` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "W", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "M", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "S", AGG = "S", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("TTG", "ATT", "ATC", "ATA", "ATG", "GTG"),
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Invertebrate Mitochondrial"),
    `6` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "Q", TAG = "Q", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = "TGA",
        Full_name = "Ciliate Nuclear; Dasycladacean Nuclear; Hexamita Nuclear"),
    `9` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "W", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "N", AAG = "K", AGT = "S", AGC = "S",
          AGA = "S", AGG = "S", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("ATG", "GTG"),
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Echinoderm Mitochondrial; Flatworm Mitochondrial"),
    `10` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "C", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Euplotid Nuclear"),
    `11` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("TTG", "CTG", "ATT", "ATC", "ATA", "ATG", "GTG"),
        Codon_stop = c("TAA", "TAG", "TGA"),
        Full_name = "Bacterial, Archaeal and Plant Plastid"),
    `12` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "S", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("CTG", "ATG"),
        Codon_stop = c("TAA", "TAG", "TGA"),
        Full_name = "Alternative Yeast Nuclear"),
    `13` = structure(
        c(TTT = "F",
          TTC = "F", TTA = "L", TTG = "L", TCT = "S", TCC = "S", TCA = "S",
          TCG = "S", TAT = "Y", TAC = "Y", TAA = "*", TAG = "*", TGT = "C",
          TGC = "C", TGA = "W", TGG = "W", CTT = "L", CTC = "L", CTA = "L",
          CTG = "L", CCT = "P", CCC = "P", CCA = "P", CCG = "P", CAT = "H",
          CAC = "H", CAA = "Q", CAG = "Q", CGT = "R", CGC = "R", CGA = "R",
          CGG = "R", ATT = "I", ATC = "I", ATA = "M", ATG = "M", ACT = "T",
          ACC = "T", ACA = "T", ACG = "T", AAT = "N", AAC = "N", AAA = "K",
          AAG = "K", AGT = "S", AGC = "S", AGA = "G", AGG = "G", GTT = "V",
          GTC = "V", GTA = "V", GTG = "V", GCT = "A", GCC = "A", GCA = "A",
          GCG = "A", GAT = "D", GAC = "D", GAA = "E", GAG = "E", GGT = "G",
          GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("TTG", "ATA", "ATG", "GTG"),
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Ascidian Mitochondrial"),
    `14` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "Y", TAG = "*", TGT = "C", TGC = "C", TGA = "W", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "N", AAG = "K", AGT = "S", AGC = "S",
          AGA = "S", AGG = "S", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = "TAG",
        Full_name = "Alternative Flatworm Mitochondrial"),
    `15` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "Q", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = c("TAA", "TGA"),
        Full_name = "Blepharisma Macronuclear"),
    `16` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "L", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = c("TAA", "TGA"),
        Full_name = "Chlorophycean Mitochondrial"),
    `21` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "W", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "M", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "N", AAG = "K", AGT = "S", AGC = "S",
          AGA = "S", AGG = "S", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("ATG", "GTG"),
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Trematode Mitochondrial"),
    `22` = structure(
        c(TTT = "F",
          TTC = "F", TTA = "L", TTG = "L", TCT = "S", TCC = "S", TCA = "*",
          TCG = "S", TAT = "Y", TAC = "Y", TAA = "*", TAG = "L", TGT = "C",
          TGC = "C", TGA = "*", TGG = "W", CTT = "L", CTC = "L", CTA = "L",
          CTG = "L", CCT = "P", CCC = "P", CCA = "P", CCG = "P", CAT = "H",
          CAC = "H", CAA = "Q", CAG = "Q", CGT = "R", CGC = "R", CGA = "R",
          CGG = "R", ATT = "I", ATC = "I", ATA = "I", ATG = "M", ACT = "T",
          ACC = "T", ACA = "T", ACG = "T", AAT = "N", AAC = "N", AAA = "K",
          AAG = "K", AGT = "S", AGC = "S", AGA = "R", AGG = "R", GTT = "V",
          GTC = "V", GTA = "V", GTG = "V", GCT = "A", GCC = "A", GCA = "A",
          GCG = "A", GAT = "D", GAC = "D", GAA = "E", GAG = "E", GGT = "G",
          GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = c("TCA", "TAA", "TGA"),
        Full_name = "Scenedesmus obliquus Mitochondrial"),
    `23` = structure(
        c(TTT = "F", TTC = "F", TTA = "*", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("ATT", "ATG", "GTG"),
        Codon_stop = c("TTA", "TAA", "TAG", "TGA"),
        Full_name = "Thraustochytrium Mitochondrial"),
    `24` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "W", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "S", AGG = "K", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("TTG", "CTG", "ATG", "GTG"),
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Pterobranchia Mitochondrial"),
    `25` = structure(
        c(TTT = "F",
          TTC = "F", TTA = "L", TTG = "L", TCT = "S", TCC = "S", TCA = "S",
          TCG = "S", TAT = "Y", TAC = "Y", TAA = "*", TAG = "*", TGT = "C",
          TGC = "C", TGA = "G", TGG = "W", CTT = "L", CTC = "L", CTA = "L",
          CTG = "L", CCT = "P", CCC = "P", CCA = "P", CCG = "P", CAT = "H",
          CAC = "H", CAA = "Q", CAG = "Q", CGT = "R", CGC = "R", CGA = "R",
          CGG = "R", ATT = "I", ATC = "I", ATA = "I", ATG = "M", ACT = "T",
          ACC = "T", ACA = "T", ACG = "T", AAT = "N", AAC = "N", AAA = "K",
          AAG = "K", AGT = "S", AGC = "S", AGA = "R", AGG = "R", GTT = "V",
          GTC = "V", GTA = "V", GTG = "V", GCT = "A", GCC = "A", GCA = "A",
          GCG = "A", GAT = "D", GAC = "D", GAA = "E", GAG = "E", GGT = "G",
          GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("TTG", "ATG", "GTG"),
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Candidate Division SR1 and Gracilibacteria"),
    `26` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "*", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "A", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("CTG", "ATG"),
        Codon_stop = c("TAA", "TAG", "TGA"),
        Full_name = "Pachysolen tannophilus Nuclear"),
    `27` = structure(
        c(TTT = "F",
          TTC = "F", TTA = "L", TTG = "L", TCT = "S", TCC = "S", TCA = "S",
          TCG = "S", TAT = "Y", TAC = "Y", TAA = "Q", TAG = "Q", TGT = "C",
          TGC = "C", TGA = "W", TGG = "W", CTT = "L", CTC = "L", CTA = "L",
          CTG = "L", CCT = "P", CCC = "P", CCA = "P", CCG = "P", CAT = "H",
          CAC = "H", CAA = "Q", CAG = "Q", CGT = "R", CGC = "R", CGA = "R",
          CGG = "R", ATT = "I", ATC = "I", ATA = "I", ATG = "M", ACT = "T",
          ACC = "T", ACA = "T", ACG = "T", AAT = "N", AAC = "N", AAA = "K",
          AAG = "K", AGT = "S", AGC = "S", AGA = "R", AGG = "R", GTT = "V",
          GTC = "V", GTA = "V", GTG = "V", GCT = "A", GCC = "A", GCA = "A",
          GCG = "A", GAT = "D", GAC = "D", GAA = "E", GAG = "E", GGT = "G",
          GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = "TGA",
        Full_name = "Karyorelict Nuclear"),
    `28` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "Q", TAG = "Q", TGT = "C", TGC = "C", TGA = "W", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = c("TAA", "TAG", "TGA"),
        Full_name = "Condylostoma Nuclear"),
    `29` = structure(
        c(TTT = "F",
          TTC = "F", TTA = "L", TTG = "L", TCT = "S", TCC = "S", TCA = "S",
          TCG = "S", TAT = "Y", TAC = "Y", TAA = "Y", TAG = "Y", TGT = "C",
          TGC = "C", TGA = "*", TGG = "W", CTT = "L", CTC = "L", CTA = "L",
          CTG = "L", CCT = "P", CCC = "P", CCA = "P", CCG = "P", CAT = "H",
          CAC = "H", CAA = "Q", CAG = "Q", CGT = "R", CGC = "R", CGA = "R",
          CGG = "R", ATT = "I", ATC = "I", ATA = "I", ATG = "M", ACT = "T",
          ACC = "T", ACA = "T", ACG = "T", AAT = "N", AAC = "N", AAA = "K",
          AAG = "K", AGT = "S", AGC = "S", AGA = "R", AGG = "R", GTT = "V",
          GTC = "V", GTA = "V", GTG = "V", GCT = "A", GCC = "A", GCA = "A",
          GCG = "A", GAT = "D", GAC = "D", GAA = "E", GAG = "E", GGT = "G",
          GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = "TGA",
        Full_name = "Mesodinium Nuclear"),
    `30` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "E", TAG = "E", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = "TGA",
        Full_name = "Peritrich Nuclear"),
    `31` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "E", TAG = "E", TGT = "C", TGC = "C", TGA = "W", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = "ATG",
        Codon_stop = c("TAA", "TAG"),
        Full_name = "Blastocrithidia Nuclear"),
    `32` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "*", TAG = "W", TGT = "C", TGC = "C", TGA = "*", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "R", AGG = "R", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("TTG", "CTG", "ATT", "ATC", "ATA", "ATG", "GTG"),
        Codon_stop = c("TAA", "TGA"),
        Full_name = "Balanophoraceae Plastid"),
    `33` = structure(
        c(TTT = "F", TTC = "F", TTA = "L", TTG = "L",
          TCT = "S", TCC = "S", TCA = "S", TCG = "S", TAT = "Y", TAC = "Y",
          TAA = "Y", TAG = "*", TGT = "C", TGC = "C", TGA = "W", TGG = "W",
          CTT = "L", CTC = "L", CTA = "L", CTG = "L", CCT = "P", CCC = "P",
          CCA = "P", CCG = "P", CAT = "H", CAC = "H", CAA = "Q", CAG = "Q",
          CGT = "R", CGC = "R", CGA = "R", CGG = "R", ATT = "I", ATC = "I",
          ATA = "I", ATG = "M", ACT = "T", ACC = "T", ACA = "T", ACG = "T",
          AAT = "N", AAC = "N", AAA = "K", AAG = "K", AGT = "S", AGC = "S",
          AGA = "S", AGG = "K", GTT = "V", GTC = "V", GTA = "V", GTG = "V",
          GCT = "A", GCC = "A", GCA = "A", GCG = "A", GAT = "D", GAC = "D",
          GAA = "E", GAG = "E", GGT = "G", GGC = "G", GGA = "G", GGG = "G"),
        Codon_start = c("TTG", "CTG", "ATG", "GTG"),
        Codon_stop = "TAG",
        Full_name = "Cephalodiscidae Mitochondrial"))

}
