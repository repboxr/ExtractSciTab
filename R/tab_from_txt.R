
example = function() {
  library(ExtractSciTab)
  library(stringtools)

  library(repboxGPT)
  project.dir = "C:/libraries/gpt/aer_112_9_2"
  page_df = ejd_load_art_pages(project.dir)
  library(ExtractSciTab)
  txt = paste0(page_df$txt, collapse="\n")
  res_li = lapply(page_df$txt,tab_from_txt)


  file = "~/test_tab2.txt"
  file = "~/articles_txt2/aer_98_3_22.txt"
  file = "~/articles_txt2/aejapp_10_1_8.txt"

  file = "~/articles_txt2/restat_100_1_10.txt"
  file = "~/articles_txt2/aejapp_6_3_7.txt"
  file = "~/articles_txt2/aer_101_6_9.txt"
  file = "~/articles_txt2/jpe_126_5_4.txt"
  file = "/home/rstudio/statabox/supp/aer_vol_101_issue_6_article_9/repbox/arttxt/aer_101_6_9.txt"

  file.copy(file, "~/ExtractSciTab/tabtest.txt",overwrite = TRUE)
  file = "~/ExtractSciTab/art.txt"
  file = "~/ExtractSciTab/test.txt"

  id = tools::file_path_sans_ext(basename(file))
  txt = readLines(file, warn=FALSE)
  res = extract_tables_from_text(txt = txt)
  tab.df = res$tabs.df
}

extract_tables_from_text = function(txt = readLines(file), file=NULL) {
  restore.point("extract_tables_from_text")

  txt = stri_replace_all_regex(txt,"[−–]","-")
  #txt = gsub("−","-",txt, fixed=TRUE)
  #txt = gsub("–","-",txt, fixed=TRUE)
  if (length(txt)==1) {
    mtxt = txt
    txt = sep.lines(txt)
  } else {
    mtxt = merge.lines(txt)
  }

  res = make.loc.line.df(mtxt)
  if (is.null(res)) {
    return(NULL)
  }
  loc.df = res$loc.df; line.df = res$line.df
  res2 = find.tab.cand(loc.df, line.df)
  if (is.null(res2)) {
    return(NULL)
  }

  loc.df = res2$loc.df#; line.df = res2$line.df; tab.df = res2$tab.df
  loc.df = split.tab.cand.panels(txt, loc.df)
  loc.df = add.tab.cols(loc.df)

  loc.df = remove.false.tables(loc.df)

  #eloc.df = tab.cols.extra.cells(txt,loc.df)
  #loc.df = bind_rows(loc.df, eloc.df)

  tabs.df = create.tabs.df(txt, loc.df)

  if (!is.null(tabs.df)) {
    tabs.df = add.tabs.source.start.end.lines(tabs.df, txt, add.above = 10, add.below = 10)
  }

  #viewCheckTabApp(txt, loc.df, add.below=10)
  list(txt=txt, tabs.df = tabs.df, loc.df = loc.df)
}


# Find locations of numbers
# Also store stars, (), [] or {} around them
# Info saved in loc.df
# line.df aggregates info to lines
make.loc.line.df = function(txt) {
  restore.point("make.loc.line.df")

  # Regex to match floting point,
  # potentially with , as 1000 separator
  # and one space between - and number
  fp = "([+\\-−]|([+\\-−] ))?(([0-9]*(,[0-9][0-9][0-9])+|([0-9]+))([.][0-9]*)?|[.][0-9]+)"
  stars = "((\\*)+|(\\+))"

  # Sometimes a little 1 is used as 10% signifiance indicator
  # after a bracket
  # e.g. (0.035)1
  one.star = "([1](?![a-zA-Z0-9]))?"
  perc = "[%]?"

  re.num = fp
  #pos = stri_locate_all_regex(txt, re.num)[[1]]

  re.paren.left = "([\\(\\[\\{][ ]?)?"
  re.paren.right = "([ ]?[\\)\\]\\}])?"
  re.big = paste0(re.paren.left,fp,"[ ]?",perc,stars,"?",re.paren.right,one.star,stars,"?")
  re.with.space = paste0("[ ]*", re.big,"[ ]*")

  loc.df = stri_locate_all_regex(txt, re.with.space)[[1]] %>%
    loc_to_df(txt,.)

  # Mark braces and remove fake numbers that either just open
  # or just close the brace.
  loc.df = loc.df %>% mutate(
    open_paren = stri_extract_first_regex(str, "[\\(\\[\\{]") %>% na.val(""),
    close_paren = stri_extract_first_regex(str, "[\\)\\]\\}]") %>% na.val("")
  ) %>%
    filter(
      (open_paren == close_paren) |
      (open_paren == "(" & close_paren == ")") |
      (open_paren == "[" & close_paren == "]") |
      (open_paren == "{" & close_paren == "}")
    ) %>%
    mutate(paren_type = open_paren)

  str = loc.df$str

  # Find spaces
  space.left = stri_locate_first_regex(str,"^[ ]*")[,2]
  space.right.mat = stri_locate_first_regex(str,"[ ]*$")
  space.right = nchar(str)+1-space.right.mat[,1]

  # Find star pos
  istar.pos = stri_locate_first_regex(str, stars)

  loc.df = loc_sep_lines(txt, loc.df)

  loc.df = loc.df %>%
    mutate(
      type = "num",
      num.str = stri_extract_first_regex(str,fp) %>% normalize.num.str(),
      num.val = suppressWarnings(as.numeric(num.str)),
      big.start = start+space.left,
      big.end = end-space.right,
      big.str = str.remove.ends(str, space.left, space.right),

      space.left = space.left,
      space.right = space.right,
      space.start = start,
      space.end = end,
      space.str = str,

      has.extra.space = space.left > 1 | space.right > 1,

      star.start = start + istar.pos[,1]-1,
      star.end = start + istar.pos[,2]-1,
      star.str = substring(str, istar.pos[,1], istar.pos[,2]),
      start = big.start,
      end = big.end,
      str = big.str
    ) %>%
    filter(!is.na(num.val)) %>%
    group_by(line) %>%
    mutate(
      has.loc.left = is.true(space.start-1 <= lag(space.end)),
      has.loc.right = is.true(space.end+1 >= lead(space.start)),
      loc.neighbours = has.loc.left + has.loc.right,
      space.left = ifelse(has.loc.left, space.left+lag(space.right), space.left)
    ) %>%
    ungroup()

  txt = sep.lines(txt)
  line.info = tibble(line=seq_along(txt),line.len = nchar(txt), ends.with.text = ends.with.text(txt))

  loc.df = left_join(loc.df, line.info, by="line")

  loc.df = loc.df %>%
    mutate(
      ends.line = space.end == line.len,
      has.perc = has.substr(big.str,"%"),
      has.star = is.true(!is.na(star.str) & !star.str=="%")
    )

  line.agg = loc.df %>%
    group_by(line) %>%
    summarize(
      num.count = n(),
      extra.space.count = sum(has.extra.space),
      spacey.neighbours.count = sum(has.extra.space & loc.neighbours>0)
    )
  line.df = line.info %>%
    left_join(line.agg, by="line") %>%
    mutate(has.num = is.true(num.count>0)) %>%
    mutate(txt = txt)

  list(loc.df = loc.df, line.df = line.df)
}

normalize.num.str = function(num.str) {
  num.str = gsub(" ","", num.str, fixed=TRUE)
  num.str = gsub(",","", num.str, fixed=TRUE)
  num.str
}



# Find candidate lines for a table
#
# Later we can refine our findings.
# E.g. merge candidates or remove some

# Criteria for a table candidate:

# One of the following two conditions is satisfied:
# a) We have at least two locations in a row whose spaces meet or overlap.
# b) In two subsequent lines, we have at loc with extra space.
find.tab.cand = function(loc.df, line.df, max_empty_row=1) {
  restore.point("find.tab.cand")
  line.df = remove.cols(line.df, c("countnum", "tpid"))
  loc.df = remove.cols(loc.df, c("tpid"))

  line.df = line.df %>%
    mutate(is.tab = is.true(
      (spacey.neighbours.count > 0) |
      ((extra.space.count > 0) & (lag(extra.space.count>0) | lead(extra.space.count>0))))) %>%
    # Consider single line breaks still part of table
    mutate(
      is.tab.for.run = is.true(is.tab | (lag(is.tab) & lead(is.tab)))
    )

  # Find tab.ind using rle
  res = rle(line.df$is.tab.for.run)
  tpid = cumsum(res$values)*res$values
  line.df$tpid= rep(tpid, times = res$lengths )

  tab.df = line.df %>%
    filter(tpid > 0)

  if (NROW(tab.df)==0) {
    return(NULL)
  }

  tab.df = tab.df %>%
    group_by(tpid) %>%
    summarize(
      tab.start = min(line),
      tab.end = max(line),
      tab.lines = tab.end-tab.start+1
    )
  loc.df = left_join(loc.df, select(line.df, line, tpid), by="line")
  list(line.df = line.df,loc.df = loc.df, tab.df = tab.df)
}

split.tab.cand.panels = function(txt, loc.df) {
  restore.point("split.tab.cand.panels")
  tpids = setdiff(unique(loc.df$tpid),0)

  tab.li = split(loc.df, loc.df$tpid)

  nloc.df = bind_rows(lapply(tab.li, function(ldf) {
    if (NROW(ldf)==0) return(ldf)
    if (first(ldf$tpid)==0) return(ldf)

    start.line = min(ldf$line)
    end.line = max(ldf$line)
    panel.lines = which(trimws(txt[start.line:end.line]) %>% startsWith("Panel ")) + start.line -1

    # Remove from panel lines that have numbers with more than one left space
    # This indicates that "Panel " might be the start of
    # a regular variable name
    left.spacy.lines = ldf$line[ldf$space.left > 1]
    panel.lines = sort(setdiff(panel.lines, left.spacy.lines))

    if (length(panel.lines)>0) {
      ldf = filter(ldf, !line %in% panel.lines)
      for (pl in panel.lines) {
        rows = ldf$line > pl
        ldf$tpid[rows] = ldf$tpid[rows]+0.001
      }
    }
    return(ldf)
  }))

  tpids = sort(unique(nloc.df$tpid))
  if (any(tpids==0)) {
    nloc.df$tpid = match(nloc.df$tpid, tpids)-1
  } else {
    nloc.df$tpid = match(nloc.df$tpid, tpids)
  }

  return(nloc.df)

}



add.tab.cols = function(loc.df) {
  restore.point("add.tab.cols")
  tpids = setdiff(unique(loc.df$tpid),0)

  loc.df$col = 0
  tpid = 3
  for (tpid in tpids) {
    rows = which(loc.df$tpid == tpid)
    tloc.df = loc.df[rows,,drop=FALSE]

    col.df = find.columns(tloc.df)

    tloc.df$col[col.df$row] = col.df$col
    tloc.df = remove.false.tab.cols(tloc.df, remove.val=-1)
    loc.df$col[rows] = tloc.df$col
  }
  # Remove locations from removed columns
  # idea: they are part of variable labels.
  # Possibly, we want to do something more sophisticated.
  loc.df = filter(loc.df, !is.true(col==-1))
  loc.df
}


find.columns = function(tloc.df,by=c("start","end")) {
  restore.point("find.columns")
  find.row.col = function(row, col=1) {
    m = match.by.location(tloc.df[row,,drop=FALSE], locs.dt, by=by)
    tibble(org.col=col,seed.row=row, row=locs.dt$.ROW[m$yid], len = NROW(m))
  }
  tloc.df$start=tloc.df$big.start
  tloc.df$end=tloc.df$big.end

  rows = seq_len(NROW(tloc.df))
  locs.dt = as.data.table(tloc.df)
  locs.dt$.ROW = rows
  by = c("start","end")
  setkeyv(locs.dt, by)


  col = 0
  res.li = vector("list",length = NROW(tloc.df))
  while (length(rows)>0) {
    col = col+1
    row = rows[1]
    res = find.row.col(row, col)
    if (! row %in% res$row) {
      stop("Own row not matched...")
    }

    res.li[[col]] = res
    rows = setdiff(rows, res$row)
  }

  res.df = bind_rows(res.li)
  # check for overlapping cols
  res.df$start = tloc.df$start[res.df$row]
  res.df$end = tloc.df$end[res.df$row]

  # If 0.25 quantiles of start and end of two columns
  # overlap, we assume they are the same column.
  changed = TRUE
  while(changed) {
    changed = FALSE
    col.se = res.df %>%
      group_by(org.col) %>%
      summarize(
        trimmed.start = quantile(start, 0.25),
        trimmed.end = quantile(end, 0.75)
      ) %>%
      arrange(trimmed.start)

    # We loop through all cols sorted by trimmed.start
    # If one change takes place. We have another
    # repetition of the while loop
    for (i in setdiff(seq_len(NROW(col.se)),NROW(col.se))) {
      if (col.se$trimmed.end[i] > col.se$trimmed.start[i+1]) {
        # Maybe we can run the while loop only once...
        changed = TRUE
        old.col = col.se$org.col[i]
        new.col = col.se$org.col[i+1]
        rows = res.df$org.col == old.col
        res.df$org.col[rows] = new.col
      }
    }
  }
  #sort(unique(res.df$org.col))


  # reorder columns
  res.df$center = (res.df$start + res.df$end)/2

  col.ord = res.df %>%
    group_by(org.col) %>%
    summarize(
      median.col.center = median(center),
      trimmed.start = quantile(start, 0.2),
      trimmed.end = quantile(end, 0.8)
    ) %>%
    mutate(col = rank(median.col.center, ties.method="first"))

  res.df = left_join(res.df, col.ord, by="org.col")


  res.df = res.df[,c("col","row","median.col.center","len","seed.row")]
  res.df = arrange(res.df, col, row)

  res.df

}


# Remove some falsely detected columns
# Using heuristics

# Problem 1: Numbers in variable labels can
#            be detected as columns
#
# Heuristic: a) Require at least one loc to have
#            at least two spaces to the left or a
#            left neighbor loc
#            b) Require all to have a left position of at least 10
remove.false.tab.cols = function(tloc.df, remove.val=-1) {
  restore.point("remove.fals.tab.cols")
  #return(tloc.df)

  # Idea: A correct column should have at least one
  #       coefficient (standard error) pair, like
  #       25.4
  #      (12.1)

  mloc.df = tloc.df %>%
    mutate(
      shaky =
        # Just a single digit is likely not a true data col
        nchar(num.str)==1 |
        (space.left <= 1 & !has.loc.left) |
        start <= 10,
      num_deci = nchar(str.right.of(num.str,".", not.found=""))
    )

  del.df = mloc.df %>%
    group_by(col) %>%
    summarize(
      del = all( (is.true(shaky) | is.na(shaky)) ) & !( any(num_deci>0 & paren_type == "") & any(num_deci >0 & paren_type !="" ))
    ) %>%
    filter(del)

  del.cols = del.df$col
  if (length(del.cols)==0) return(tloc.df)

  # Adapt column numbers
  all.cols = sort(unique(tloc.df$col))
  is.removed = all.cols %in% del.cols
  new.col = all.cols - cumsum(is.removed)
  # Locations with removed cols get a column
  # equal to remove.val
  # They may be later deleted
  new.col[is.removed] = remove.val

  tloc.df$col = new.col[tloc.df$col]

  return(tloc.df)
}

remove.false.tables = function(loc.df) {
  restore.point("remove.false.tables")
  # Remove empty tables and adapt tpids
  loc.df = loc.df %>%
    group_by(tpid) %>%
    mutate(max.col = max(col)) %>%
    filter(max.col >0) %>%
    group_by(tpid) %>%
    filter(n() > 1) %>%
    mutate(
      # Update to a less stringent definition of shaky. The old definition ruled
      # out some plausible table lines e.g. in aejmac_11_3_1 Table 1
      shaky = is.true(
        (space.left <=1 & space.right <= 1) |
        (space.left <= 1 & !has.loc.left) |
        (space.right <= 1 &  end < line.len & !has.loc.right) |
        ends.with.text
      ),
#      shaky = is.true((space.left <=1) |
#        (space.right <= 1 & end < line.len) |
#        (ends.with.text)),
      share.shaky = sum(shaky) / n()
    ) %>%
    filter(share.shaky < 0.9) %>%
    ungroup()

  tpids = sort(unique(loc.df$tpid))
  if (length(tpids)>0) {
    vec = 1:max(tpids)
    has = vec %in% tpids
    newtpid = cumsum(has)
    loc.df$tpid = newtpid[loc.df$tpid]
  }
  loc.df
}


create.tabs.df = function(txt, loc.df) {
  restore.point("create.tabs.df")
  if (NROW(loc.df)==0) return(NULL)
  tabs = seq_len(max(loc.df$tpid))
  #tabs = unique(loc.df$tpid)
  n = length(tabs)
  tabs.df = tibble(tpid=tabs, start.line=NA_integer_, end.line=NA_integer_, loc.df = vector("list",n))
  for (tab.num in tabs) {
    tloc.df = filter(loc.df, tpid==tab.num)
    tabs.df$loc.df[[tab.num]] = tloc.df
    tabs.df$start.line[tab.num] = min(tloc.df$line)
    tabs.df$end.line[tab.num] = max(tloc.df$line)
  }
  tabs.df$prev.end.line = lead(tabs.df$end.line, default=NA)
  tabs.df$next.start.line = lag(tabs.df$start.line, default=NA)

  title.df = bind_rows(lapply(tabs, function(tab.num) extract.table.title(tabs.df[tab.num,], txt)))
  tabs.df = bind_cols(tabs.df, title.df)

  tabs.df = add.groups.and.panels.to.tabs.df(tabs.df)

  tabs.df = tabs.df %>% mutate(tabfig = case_when(
    !is.na(table.title) & is.na(figure.title) ~ "tab",
    is.na(table.title) & !is.na(figure.title) ~ "fig",
    !is.na(table.title) & !is.na(figure.title) &
      table.title.space.left > 1 & figure.title.space.left <= 1 ~ "table",
    TRUE ~ "unknown"
  ))

  panel.index = function(panel) {
    restore.point("panel.index")
    if (all(panel<26)) {
      return(LETTERS[panel])
    } else {
      return(panel)
    }
  }
  tabs.df = tabs.df %>% mutate(
    tabname = ifelse(tabfig!="fig",
      extract.tabname(table.title, tabid),
      extract.figname(figure.title, tabid)
    ),
    tpname = ifelse(num.panel > 1,
      paste0(tabname,"-",panel.index(panel)),
      tabname
    )
  )

  tabs.df
}

add.groups.and.panels.to.tabs.df = function(tabs.df) {
  restore.point("add.groups.and.panels.to.tabs.df")
  merge.dist = 10
  first.non.na = function(x) {
    n = length(x)
    row = which(!is.na(x))
    if (length(row)==0) return(x)
    rep(x[row[1]],n)
  }

  tabs.df = tabs.df %>%
    mutate(merge.with.above =
      is.true(lag(table.title.line)==table.title.line) |
      is.true(lag(figure.title.line)==figure.title.line) |
      (is.na(table.title.line) & is.na(lag(figure.title.line)) & (start.line-lag(end.line) <= merge.dist))
    ) %>%
    mutate(
      merge.with.above = ifelse(is.na(merge.with.above),FALSE, merge.with.above)) %>%
    mutate(
      tabid = cumsum(!merge.with.above)
    ) %>%
    group_by(tabid) %>%
    mutate(
      num.panel = n(),
      panel = 1:n(),
      across(table.title.line:table.title.space.left, first.non.na),
      across(figure.title.line:figure.title.space.left, first.non.na)
    ) %>%
    ungroup()
  tabs.df
}

extract.tabname = function(title, tabid) {
  restore.point("extract.tabname")
  names = gsub("(Table[^a-zA-Z0-9]*[a-zA-Z0-9]+)(.*)","\\1",title,ignore.case = TRUE)
  rows = is.na(title)
  names[is.na(title)] = paste0("Unknown ", tabid[rows])
  names
}

extract.figname = function(title, tabid) {
  names = gsub("(Figure[^a-zA-Z0-9]*[a-zA-Z0-9]+)(.*)","\\1",title,ignore.case = TRUE)
  rows = is.na(title)
  names[is.na(title)] = paste0("Unknown ", tabid[rows])
  names
}


extract.table.title = function(tab,txt) {
  restore.point("extract.table.title")
  #if (tab$tpid==31) stop()
  # Analyse table title
  end.line = pmax(tab$start.line-1,1)
  start.line = pmax(tab$start.line-15, 1)
  #start.line = suppressWarnings(pmax(min(tab$start.line-10, tab$prev.start.line+1, 1, na.rm = TRUE),1))
  lines = start.line:end.line

  get.line = function(rows, lines) {
    if (length(rows)==0) return(NA_integer_)
    lines[rows[length(rows)]]
  }

  get.txt = function(line) {
    if (is.na(line)) return(NA_character_)
    trimws(txt[line])
  }

  get.space.left = function(line) {
    as.integer(regexpr("[A-Za-z]", txt[line]))-1
  }

  ttxt = trimws(txt[lines])
  has.space = startsWith(txt[lines],"   ")
  rows = which(
    startsWith(ttxt,"Table") | startsWith(ttxt,"TABLE ") |
    (has.space & (has.substr(ttxt,"Table ") | has.substr(ttxt,"TABLE ")))
  )
  table.title.line = get.line(rows, lines)

  rows = which(startsWith(ttxt,"Panel ") | startsWith(ttxt,"PANEL "))
  panel.title.line = get.line(rows, lines)
  if (isTRUE(panel.title.line < table.title.line))
    panel.title.line = NA_integer_

  rows = which(startsWith(ttxt,"Figure ") | startsWith(ttxt,"FIGURE "))
  figure.above.title.line = get.line(rows, lines)
  if (isTRUE(figure.above.title.line < table.title.line))
    figure.above.title.line = NA_integer_

  # Search for Figure title below
  lines = tab$end.line:(tab$end.line+10)
  ttxt = trimws(txt[lines])
  rows = which(startsWith(ttxt,"Figure ") | startsWith(ttxt,"FIGURE "))
  figure.title.line = get.line(rev(rows), lines)

  res = tibble(
    table.title.line = table.title.line,
    table.title = get.txt(table.title.line),
    table.title.above = tab$start.line-table.title.line,
    table.title.space.left = get.space.left(table.title.line),

    panel.title.line = panel.title.line,
    panel.title = get.txt(panel.title.line),
    panel.title.above = tab$start.line-panel.title.line,


    figure.title.line = figure.title.line,
    figure.title = get.txt(figure.title.line),
    figure.title.below = figure.title.line-tab$end.line,
    figure.title.space.left = get.space.left(figure.title.line),

    figure.above.title.line = figure.above.title.line,
    figure.above.title = get.txt(figure.above.title.line),
    figure.above.title.above = tab$start.line-figure.above.title.line,
    figure.above.title.space.left = get.space.left(figure.above.title.line),

  )
  if (NROW(res)>1) stop("To many rows in title tibble")
  return(res)
}


count.with = function(str, ...) {
  substr = c(...)

  restore.point("ksjdlkjsdlk")
  has = rep(TRUE, length(str))
  for (s in substr) {
    has = has & has.substr(str, s)
  }
  suppressWarnings(c(count = sum(has), first.line = min(which(has))))
}


count.with.mult = function(str, ...) {
  substr.li = list(...)

  has.one = rep(FALSE, length(str))

  for (substr in substr.li) {
    has = rep(TRUE, length(str))
    for (s in substr) {
      has = has & has.substr(str, s)
    }
    has.one = has.one | has
  }
  suppressWarnings(c(count = sum(has.one), first.line = min(which(has.one))))
}



text.colors = function() {
  rep(c("#000000","#1B9E77","#D95F02","#7570B3","#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#4477dd", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"),times = 4)
}


find.tab.rownames = function(txt, loc.df) {
  tabs = seq_len(max(loc.df$tpid))
  res.li = lapply(tabs, function(tab.num) {
    restore.point("find.tab.rownames.inner")
    lines = sort(unique(loc.df$line[loc.df$tpid == tab.num]))

    # Only keep now locations
    eloc.df = remove.overlapping.loc(eloc.df, tloc.df)

    # Match columns
    col.df = tloc.df %>%
      group_by(col) %>%
      summarize(
        start = min(start),
        end = max(end)
      )
    cm = match.by.location(eloc.df, col.df, mult="first", details=FALSE)
    eloc.df$tpid = tab.num
    eloc.df$col = cm$yid
    eloc.df$color = colors[eloc.df$col+1]
    eloc.df
  })


}


match.by.location = function(x, y, by=c("start","end"), details=FALSE, type="any", mult="all") {
  restore.point("match.by.location")
  library(data.table)
  if (!details & mult != "all") {
    x$.ROW = seq_len(NROW(x))
    y$.ROW = seq_len(NROW(y))
  }
  loc1 = as.data.table(x)
  setkeyv(loc1, by)

  loc2 = as.data.table(y)
  setkeyv(loc2, by)

  ol = foverlaps(loc1, loc2, by.x=by, by.y=by, which=TRUE, nomatch=NA, type=type, mult=mult)
  if (!details & mult != "all") {
    ol = tibble(xid=loc1$.ROW, yid=loc2$.ROW[ol]) %>%
      arrange(xid)
    return(ol)
  } else if (!details) {
    return(as_tibble(ol))
  }

  ol$xrow = loc1$loc.row[ol$xid]
  ol$yrow = loc2$loc.row[ol$yid]
  ol$xline = loc1$line[ol$xid]
  ol$yline = loc2$line[ol$yid]
  ol$xdupl = duplicated(ol$xid) & !is.na(ol$xid)
  ol$ydupl = duplicated(ol$yid) & !is.na(ol$yid)
  as_tibble(ol)
}


add.tabs.source.start.end.lines = function(tabs.df, txt, add.above=5, add.below=6, title.line=NA) {
  restore.point("add.tabs.source.start.end.lines")
  if (is.null(tabs.df)) return(NULL)

  restore.point("add.tabs.source.start.end.lines2")
  min.lines = sapply(tabs.df$loc.df, function(tloc.df) min(tloc.df$line))
  max.lines = sapply(tabs.df$loc.df, function(tloc.df) max(tloc.df$line))

  title.lines = na.val(tabs.df$table.title.line, Inf)

  start.lines = pmax(1,min.lines-add.above) %>% pmin(title.lines)

  end.lines = pmin(max.lines+add.below, NROW(txt))

  tabs.df$source.start.line = start.lines
  tabs.df$source.end.line = end.lines
  tabs.df
}

