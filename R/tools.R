from_to = function(from, to, min=from, max=to) {
  from = max(from, min)
  to = min(to, max)
  if (from > to) return(NULL)
  from:to
}

tabtitle_to_tabid = function(tabtitle) {
  tabname_to_tabid(tabtitle)
}

tabname_to_tabid = function(tabname) {
  restore.point("tabname_to_tabid")
  tabid = str.right.of(tabname, "able ") %>% trimws()
  tabid = gsub("\\.$","",tabid) %>% trimws()

  rows = which(startsWith(tolower(tabname),"appendix"))
  tabid[rows] = paste0("app.",tabid[rows])

  # Update: remove spaces and other special letter
  # We will later use tabid inside HTML ids: so it should
  # not have any special symbols anymore
  tabid = stringi::stri_replace_all_regex(tabid, "[^a-zA-Z0-9_]","")
  tabid = stringi::stri_replace_all_fixed(tabid, "TABLE","")
  tabid
}


has.substr = function (str, pattern) {
  stri_detect_fixed(str, pattern)
}

loc_sep_lines = function(txt, loc, start_col = "start", end_col="end", line_col="line") {
  restore.point("loc_sep_lines")
  lines_loc = stri_locate_all_fixed(txt, "\n")[[1]]
  lines_start = c(1, lines_loc[,1]+1)
  loc[[line_col]] = findInterval(loc$start, lines_start)
  loc[[start_col]] = loc$start-lines_start[loc[[line_col]]]+1
  loc[[end_col]] = loc$end-lines_start[loc[[line_col]]]+1
  loc


}

loc_sep_lines2 = function(txt, loc) {
  restore.point("loc_sep_lines")
  lines_loc = stri_locate_all_fixed(txt, "\n")[[1]]
  lines_start = c(1, lines_loc[,1]+1)
  loc$row = findInterval(loc$start, lines_start)
  loc$col_start = loc$start-lines_start[loc$row]+1
  loc$col_end = loc$end-lines_start[loc$row]+1
  loc


}

loc_to_df = function(txt,loc, add.left=0, add.right=0) {
  if (NROW(loc)==0) {
    return(tibble(start=integer(0), end=integer(0), str=character(0)))
  }

  tibble(
    start = loc[,1],
    end = loc[,2],
    # stri_sub is faster
    str = stri_sub(txt, loc[,1]-add.left, loc[,2]+add.right)
    #str = substring(txt, loc[,1]-add.left, loc[,2]+add.right)
  )
}

na.val = function(x, val=0) {
  x[is.na(x)] = val
  x
}

is.true = function(x) {
  val = x == TRUE
  val[is.na(x)] = FALSE
  val
}

most.common = function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

remove.cols = function(x, cols) {
  x = x[,setdiff(colnames(x),cols)]
}

# Remove from x all locations that overlap with a location in y
remove.overlapping.loc = function(x, y, by=c("line", "start","end"), mode=c("remove","keep","mark")[1]) {
  restore.point("remove.overlapping.loc")
  library(data.table)
  loc1 = as.data.table(x)
  setkeyv(loc1, by)

  loc2 = as.data.table(y)
  setkeyv(loc2, by)

  ol = foverlaps(loc1, loc2, by.x=by, by.y=by, which=TRUE, nomatch=NA)
  not.ol = sort(unique(ol$xid[is.na(ol$yid)]))
  if (mode=="mark") {
    x$overlapping = TRUE
    x$overlapping[!not.ol] = FALSE
    return(x)
  }
  if (mode =="remove") {
    x[not.ol,,drop=FALSE]
  } else if (mode == "keep") {
    if (length(not.ol)==0) return(x)
    x[-not.ol,,drop=FALSE]
  }
}

keep.overlapping.loc = function(...) {
  remove.overlapping.loc(..., mode="keep")
}


locate_all_as_df = function(txt, regexpr.li) {
  restore.point("locate_all_as_df")
  locs.li = lapply(regexpr.li, function(regexpr) {
    str_locate_all(txt, regexpr)
  })

  loc.df = bind_rows(lapply(seq_along(locs.li), function(i) {
    type = names(locs.li)[i]
    locs = bind_rows(lapply(seq_along(locs.li[[i]]), function(row) {
      loc = locs.li[[i]][[row]]
      if (NROW(loc)==0) return(NULL)
      loc = as_tibble(loc)
      loc$line = row
      loc$type = type
      loc$pos = 1:NROW(loc)
      loc
    }))
    locs
  }))
  if (NROW(loc.df)==0) {
    loc.df = tibble(start=integer(0),end=integer(0), line=integer(0), type=character(0), pos=integer(0))
  }

  loc.df$str = substring(txt[loc.df$line], loc.df$start, loc.df$end)
  loc.df$nchar = loc.df$end-loc.df$start
  loc.df
}


ends.with.text = function(txt, end.size=4) {
  str = trimws(txt)
  rhs = substring(str, nchar(str)-end.size,nchar(str))
  grepl("[a-zA-z]",rhs,fixed=FALSE)

}

seq_rows = function(x) {
  seq_len(NROW(x))
}

combine_text_lines = function(txt, remove.line.end.hyphen = TRUE, replace.line.breaks=TRUE, remove_repeated_ws = TRUE) {
  restore.point("combine_text_lines")
  txt = merge.lines(txt)
  if (remove.line.end.hyphen) {
    if (replace.line.breaks) {
      txt = stri_replace_all_fixed(txt, "-\n","")
    } else {
      txt = stri_replace_all_fixed(txt, "-\n","\n")
    }
  }
  if (replace.line.breaks) {
    # Replace line breaks that are not followed by a space or another line break
    # This means we only want to keep line breaks for likely paragraph breaks.
    txt = stri_replace_all_regex(txt, "\n(?![ \n])"," ")

    # Also replace line breaks if last letter in previous line was
    # a simple lower case character. Then we don't have a finished sentence
    # and no new paragraph should start
    txt = stri_replace_all_regex(txt, "(?<=[a-z])\n"," ")
  }
  if (remove_repeated_ws) {
    txt = stri_replace_all_regex(txt,"[ ]+"," ")
  }

  txt
}

left_join_overlap = function (x, y, by_x, by_y, mult = "all", type = "any", nomatch = NA) {
  dt_x = as.data.table(x)
  dt_y = as.data.table(y, key = by_y)
  dt_ol = foverlaps(dt_x, dt_y, mult = mult, type = type, which = FALSE,
                    by.x = by_x)
  as_tibble(dt_ol)
}

