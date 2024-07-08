# A lot of code was copied from reboxArt
#
# The original function extract_tables_from_text
# is too limited
#
# We extract tables with the following components:
#
# tabid:
# tabtitle:
# tabhtml:
# tabnotes:
# cell_df:



example = function() {
  library(ExtractSciTab)
  project_dir = "/home/rstudio/repbox/projects_gha/restud_86_2_1"

  page_df = readRDS(file.path(project_dir,"art/txt_pages.Rds"))

  txt = page_df$txt[page_df$page==25]
  #txt = paste0(page_df$txt, collapse="\n")
  #txt = readLines(paste0(project_dir,"/art/art.txt"))
  raw = extract_raw_tables_from_text(txt = txt)

  tab_df = refine_raw_tab(raw)

  cell_df = tab_df$cell_df[[1]]
  html = tab_df$tab_html[[which(tab_df$tabid=="5")]]

  repboxArt::show_cell_df_html(tab_df$cell_df[[1]],color_by = "col")
  writeLines(html, "~/repbox/temp/table.html")

  browseURL("~/repbox/temp/table.html")

  rstudioapi::filesPaneNavigate(project_dir)
  tp_df = readRDS(paste0(project_dir,"/art/art_tab_raw.Rds"))

}


extract_tables_from_art_text = function(txt) {
  txt = merge.lines(txt)
  raw = extract_raw_tables_from_text(txt = txt)

  refine_raw_art_tab(raw)
}

refine_raw_tab = function(raw, page_df=NULL) {
  restore.point("refine_raw_tp_df")
  tp_df = raw$tabs.df
  txt   = raw$txt

  if (NROW(tp_df)==0) return(NULL)


  names(tp_df) = gsub(".","_", names(tp_df), fixed=TRUE)

  colnames(tp_df)
  cols = union(c("tabid", "tpid","tabname","tpname","tabtitle","start_line","end_line"), colnames(tp_df))
  tp_df = tp_df[,cols]


  panel_letters = c(LETTERS, paste0("Z", LETTERS), paste0("ZZ",LETTERS))
  # Only keep rows that can be mapped to a well-defined table
  # with Table XXX in the title.
  tp_df = tp_df %>%
    filter(tabfig == "tab") %>%
    # We then use the official table number as tabid
    mutate(
      tabid = tabname_to_tabid(tabname)
    ) %>%
    group_by(tabid) %>%
    mutate(
      tpid = paste0(tabid, "-",panel_letters[1:n()])
    ) %>%
    ungroup()
  tabids = unique(tp_df$tabid)
  tp_df$tabpos = match(tp_df$tabid, tabids)

  for (i in seq_len(NROW(tp_df))) {
    if (NROW(tp_df$loc_df[[i]])>0) {
      tp_df$loc_df[[i]]$tpid = tp_df$tpid[[i]]
    }
  }

  tp_df = tp_df %>%
    rename(tptitle = panel_title)

  # tp_df can contain text parts that are wrongly identified as tables
  # For every part try to determine the best.
  tp_df = tp_df %>%
    mutate(
      pad_tabid = pad_tabid(tabid),
      #chunk_ind = cumsum(!is.true(lag(tabid) == tabid))

      # also conditioning on tabname better rules out some text chunks
      # referring to the table
      chunk_ind = cumsum(!is.true(lag(tabid) == tabid & lag(tabname)==tabname))
    )

  chunk_df = tp_df %>%
    group_by(chunk_ind, tabid, pad_tabid) %>%
    summarize(
      num_lines = max(end_line)-min(start_line)+1
    ) %>%
    ungroup() %>%
    mutate(
      prev_bigger = is.true(lag(pad_tabid)>pad_tabid),
      next_smaller = is.true(lead(pad_tabid)<pad_tabid)
    )
  keep_chunk_df = chunk_df %>%
    group_by(tabid) %>%
    arrange(desc(num_lines), prev_bigger+next_smaller) %>%
    slice(1)

  tp_df = semi_join(tp_df, keep_chunk_df, by = "chunk_ind")


  # In the moment, we will ignore table parts that continue on later pages
  # often these are false positives or inbetween there is still text.
  # Better code would identify them

  # tp_df = tp_df %>%
  #   group_by(tabid) %>%
  #   mutate(same_page = start_page == first(start_page)) %>%
  #   filter(same_page)


  tab_df = tp_df %>%
    group_by(tabid) %>%
    summarize(
      start_line = min(c(start_line, tabtitle_line),na.rm = TRUE),
      end_line = max(c(end_line),na.rm=TRUE),
      title_line = min(tabtitle_line, na.rm=TRUE)
    )

  # NEW: Merge tp_df for each tab and
  #      compute columns again
  tp_df = tp_df %>%
    group_by(tabid, pad_tabid) %>%
    summarize(
      tpid = first(tpid),
      panel_title = "",
      tptitle = first(tptitle),
      tpname = first(tpname),
      tabtitle_line = first(tabtitle_line),
      start_line = min(c(start_line, tabtitle_line),na.rm = TRUE),
      end_line = max(c(end_line),na.rm=TRUE),
      title_line = min(tabtitle_line, na.rm=TRUE),
      loc_df = list(merge_loc_df(loc_df))
    )


  sep_txt = sep.lines(txt)
  line_df = tibble(line=seq_along(sep_txt), txt=sep_txt, trim_txt = trimws(sep_txt), keep=TRUE, type = "", page = 1, tabid="",footind=0) %>%
    group_by(page) %>%
    mutate(
      pline = 1:n(),
      rev_pline = (n():1)
    ) %>%
    ungroup()


  line_df = line_df_find_table_parts(line_df, tab_df)

  # Note: tab_parts have not yet specified cell_df
  tab_parts = line_df_extract_table_parts(line_df)
  tab_parts$row_df = tab_parts$panel_df = vector("list",NROW(tab_parts))


  i = 1
  for (i in seq_rows(tab_parts)) {
    cell_df = make_tab_cell_df(tab_parts[i,],tp_df[tp_df$tabid==tab_parts$tabid[[i]],])
    tab_parts$cell_df[[i]] = cell_df
    #tab_parts$row_df[[i]] = res$row_df
    #tab_parts$panel_df[[i]] = res$panel_df
  }

  tab_parts$tab_html = sapply(tab_parts$cell_df, convert_cell_df_to_tabhtml)

  table_df = left_join(tab_parts,tab_df, by = "tabid")
  table_df = table_df[, setdiff(names(table_df),c("start_page"))]
  table_df$pad_tabid = pad_tabid(table_df$tabid)
  table_df
}

merge_loc_df = function(loc_df_li) {
  restore.point("merge_loc_df")
  loc_df = bind_rows(loc_df_li)
  #loc_df$org_tpid = loc_df$tpid
  loc_df$tpid = first(loc_df$tpid)
  if (length(loc_df_li)>1) {
    loc_df = add.tab.cols(loc_df)
  }
  loc_df$max.col = max(loc_df$col)
  loc_df
}


convert_cell_df_to_tabhtml = function(cell_df) {
  tr_df = cell_df %>%
    group_by(row) %>%
    summarize(
      html = paste0('<tr data-row="', first(row),'">\n',
        paste0('  <td data-row="', row,'" data-col="',col,'"',
               ifelse(is.true(colspan>1),paste0('colspan="', colspan,'"'), ''),
               '>',text, "</td>", collapse = "\n"),
        '\n</tr>'
      )
    )
  tabhtml = paste0("<table>\n",paste0(tr_df$html, collapse="\n"),"</table>")
  tabhtml
}


# Create cell_df from tp_df
# tp_df has extracted numbers, but we want to recreate
# as close as possible also the text cells
make_tab_cell_df = function(tab, tp_df) {
  restore.point("make_tab_cel_df")
  #if (tab$tabid=="3") stop()


  txt = tab$tabsource %>%
    stri_replace_all_regex("[−–]","-")
  #stri_replace_all_fixed("−","-")

  # This regular expression is used by scitab
  # We want to directly map all those expressions
  # instead of single words so that we can remove them from word_loc
  scitab_num_rx = "([\\(\\[\\{][ ]?)?([+\\-−]|([+\\-−] ))?(([0-9]*(,[0-9][0-9][0-9])+|([0-9]+))([.][0-9]*)?|[.][0-9]+)[ ]?[%]?((\\*)+|(\\+))?([ ]?[\\)\\]\\}])?([1](?![a-zA-Z0-9]))?((\\*)+|(\\+))?"
  rx = paste0("(",scitab_num_rx,")|([^ \n]+)")

  word_loc = stri_locate_all_regex(txt,rx) [[1]]
  word_loc = loc_to_df(txt, word_loc)

  word_loc = loc_sep_lines2(txt, word_loc) %>%
    select(str=str, row, start=col_start, end=col_end) %>%
    mutate(str = trimws(str))

  word_loc$col = NA_integer_

  cell_df_li = vector("list",NROW(tp_df))
  i = 1
  skipped = FALSE
  for (i in seq_len(NROW(tp_df))) {


    res = tp_to_cell_df(tp_df[i,], word_loc,add_words_below = (i==NROW(tp_df)),panel_num = i)
    cell_df = res$cell_df

    # Just in case all numbers were considered to be part of the text
    # and there is another panel, then we just add this panel to the other
    # panel. This is the case in aejapp_3_2_2 Table 1, panel 2
    if (i < NROW(tp_df)) {
      if (all(cell_df$type=="text")) {
        skipped = TRUE
        next
      }
    }

    cell_df_li[[i]] = cell_df
    if (i == NROW(tp_df)) break
    word_loc = res$word_below
  }
  cell_df = bind_rows(cell_df_li)

  # If some panels were skipped
  if (skipped) {
    panel_nums = sort(unique(cell_df$panel_num))
    cell_df$panel_num = match(cell_df$panel_num, panel_nums)
  }


  # Fill in missing cells as empty
  all_cells = expand.grid(row = unique(cell_df$row), col=unique(cell_df$col)) %>% mutate(
    type="text", text = "", colspan = 1
  )
  all_cells = anti_join(all_cells, cell_df, by=c("row","col"))

  cell_df = bind_rows(cell_df, all_cells) %>%
    arrange(row, col)

  # We merge cells and extend colspan if both cells are
  # type=="text" and either
  # i) the right cell is an empty text or
  # ii) there is only a single space between both cells
  while(TRUE) {
    cell_df = cell_df %>%
      group_by(row) %>%
      mutate(
        merge_with_left = is.true(
          type=="text" & lag(type)=="text" &
            (text == "" |  is.true(start-lag(end) <= 2))
        ),
        merge_with_right= is.true(lead(merge_with_left))
      ) %>%
      # In a chain do only a single merge at a time
      mutate(merge_with_left = merge_with_left & !merge_with_right)

    if (!any(cell_df$merge_with_left)) break

    # Perform merge
    rows = which(cell_df$merge_with_left)
    cell_df$colspan[rows-1] = cell_df$col[rows] - cell_df$col[rows-1] + cell_df$colspan[rows]

    rows2 = which(cell_df$merge_with_left & cell_df$text != "")
    cell_df$text[rows2-1] = paste0(cell_df$text[rows2-1]," ",cell_df$text[rows2])
    cell_df$end[rows2-1] = cell_df$end[rows2]

    # Remove right hand side cells that have been merged
    cell_df = cell_df[-rows,]
  }

  cell_df = cell_df %>%
    arrange(row, col) %>%
    group_by(row) %>%
    mutate(
      colspan=case_when(
        is.na(lead(col)) ~ max.col-col+1,
        TRUE ~ pmax(1,lead(col)-col)
      )
    )



  cell_df = cell_df %>%
    mutate(
      is_int_header = type == "num" & num_deci==0 & paren_type=="("
    )

  return(cell_df)

  # row_df = cell_df %>%
  #   group_by(row) %>%
  #   summarise(
  #     is_int_header_block = sum(is_int_header)>0 & sum(!is_int_header==0),
  #     is_empty_row = all(type=="empty"),
  #     is_num_row = any(type=="num"),
  #     rowname = case_when(
  #       first(type)== "text" ~ first(text),
  #       first(type)=="empty" & nth(type,2,default="") == "text" ~ nth(text,2),
  #       TRUE ~ ""
  #     ),
  #     panel_num = first(panel_num)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     .new_panel_num = !is.true(lag(panel_num)==panel_num),
  #     .new_num_block = !is.true(lag(is_num_row) | !.new_panel_num),
  #     num_row_block = cumsum(.new_num_block)*is_num_row
  #   ) %>%
  #   select(-.new_panel_num, -.new_num_block)
  #
  # panel_df = tp_df %>%
  #   transmute(panel_title=ifelse(is.na(tptitle), tpname, tptitle), panel_num=seq_len(n()))
  #
  # cell_df = left_join(cell_df, select(row_df, row, num_row_block), by="row")
  # list(cell_df=cell_df, row_df=row_df, panel_df=panel_df)


}


# The raw extraction in ExtractSciTab did not add text cells properly
# this function does so. On the other hand, the original column
# detection of ExtractSciTab for num cells is relatively
# sophisticated (works also with slightly missarranged columns)
# So we want to reuse their column positions
tp_to_cell_df = function(tp, word_loc, add_words_below=FALSE, panel_num=0) {
  restore.point("tp_to_cell_df")
  #if (tp$tabid=="3") stop()
  loc_df = tp$loc_df[[1]] %>%
    filter(type == "num") %>%
    mutate(row = line-tp$tabtitle_line)

  loc_df = loc_df %>%
    rename(num_str = num.str, num = num.val) %>%
    mutate(
      str = trimws(str),
      num_deci = nchar(str.right.of(num_str,".", not.found=rep("", n()))),
      stars_str = find_stars_str(str)
    )

  # Try to detect integer header rows (1)  (2)  (3)
  loc_df = loc_df %>%
    group_by(row) %>%
    mutate(
      is_int_header = n() > 1 & all(paren_type == "(" & num_deci == 0) & all(diff(num)==1)
    )

  # Newly introduced: need to check
  loc_df = split_multi_cell_cols(loc_df)

  loc_df = repair_slipped_cols(loc_df)
  loc_df = repair_same_col_cells(loc_df)

  max_row = max(loc_df$row)
  word_loc$is_below = word_loc$row > max_row
  if (!add_words_below) {
    words_below = filter(word_loc, is_below)
    word_loc = filter(word_loc, !is_below)
  } else {
    words_below = NULL
  }

  # Remove words that are already in loc_df
  # Note that we adapted the rx for word loc so that
  # we find the same text as loc_df$big.str
  word_loc = anti_join(word_loc, loc_df, by=c(str="big.str"))

  col_df = loc_df %>%
    group_by(col) %>%
    summarize(
      sstart = min(big.start),
      send = max(big.end)
    ) %>%
    arrange(col) %>%
    mutate(
      start = ifelse(is.na(lag(send)),sstart,
                     sstart - floor( (sstart-lag(send)) / 2 )
      ),
      end = ifelse(is.na(lead(sstart)),send,
                   send + ceiling( (lead(sstart)-send) / 2 )
      )
    ) %>%
    select(col, col_start=start,col_end=end)
  col_df$col_end[NROW(col_df)] = col_df$col_end[NROW(col_df)]+1000

  if (first(col_df$col_start)>1) {
    col_df = bind_rows(
      tibble(col=0, col_start=1, col_end=first(col_df$col_start)-1),
      col_df
    )
  }

  # We define also narrower intervals
  col2_df = loc_df %>%
    group_by(col) %>%
    summarize(
      col_start = floor(mean(big.start)),
      col_end = ceiling(mean(big.end))
    )
  col3_df = col2_df %>%
    group_by(col) %>%
    summarize(
      col_start = floor((col_start+col_end)/2)-1,
      col_end = col_start +3
    )

  # Find column(s) with which words overlap
  word_loc = left_join_overlap(word_loc, bind_rows(col_df, col2_df,col3_df), c("start","end"), c("col_start","col_end"))

  # We kick out matches that have same row and col as a cell in loc_df
  # Normally there should be no matches but handling that case
  # would be really difficult. Now in worst case, just some
  # explanatory text gets lost in out cell_df
  word_loc = anti_join(word_loc, loc_df, by = c("row","col"))

  # Now we take the column with the most hits
  w_loc = word_loc %>%
    group_by(row, start, end, str, col) %>%
    summarize(
      num_col_match = n()
    ) %>%
    group_by(row, start, end, str) %>%
    arrange(desc(num_col_match),col) %>%
    slice(1) %>%
    ungroup()

  # Combine multiple strings in same column
  w_loc = w_loc %>%
    mutate(str = trimws(str)) %>%
    group_by(row, col) %>%
    summarize(
      str = paste0(str, collapse=" "),
      start = suppressWarnings(min(start)),
      end = suppressWarnings(max(end)),
      colspan = 1
    ) %>%
    ungroup()

  w_loc$type = "text"

  opts = list(few_integer_as_text_limit=5)

  # Sometimes we have a panel that is just some text inbetween and all
  # numbers are just text. Try to detect such panels with the conditions below.
  if (NROW(loc_df)<=opts$few_integer_as_text_limit) {
    if (all(loc_df$num_deci == 0 & loc_df$num >= 0 & (loc_df$num <= 100 | (loc_df$num >= 1800 & loc_df$num <= 2200) | loc_df$num %in% c(1000,10000)) )) {
      loc_df$type = "text"
    }
  }

  cols = c(colnames(w_loc),c("num_str", "num","num_deci", "stars_str","paren_type","is_int_header"))
  cell_df = bind_rows(
    loc_df,
    w_loc
  )[,cols]

  if (isTRUE(min(cell_df$col)==0)) {
    cell_df$col = cell_df$col + 1
  }

  cell_df = cell_df %>%
    rename(text = str)

  # If there are cells with same row and col remaining, ensure that
  # they are separated
  cell_df = split_multi_cell_cols(cell_df,sep_all = TRUE)
  cell_df = merge_single_neg_int_cols(cell_df)


  cell_df$type[is.true(cell_df$is_int_header)] = "text"


  # Try to detect numbers that are only part of the header and say that
  # they are text. We may loose with small probability some number,
  # But that is likely not too bad.
  suppressWarnings({
    first.deci.row <- min(loc_df$row[loc_df$num_deci >0])
    int.header.row <- min(loc_df$row[loc_df$is_int_header])
    above.text.row <- max(c(first.deci.row, int.header.row)-1,na.rm = TRUE)
    if (!is.finite(above.text.row)) {
      #above.text.row <- max(cell_df$row)
      above.text.row <- 0
    }
  })
  if (above.text.row > 0) {
    inds = cell_df$type == "num" & cell_df$row <= above.text.row
    cell_df$type[inds] = "text"
  }

  cell_df$panel_num = panel_num


  # Finally arrange by row and col and adapt colspan
  cell_df = cell_df %>%
    arrange(row, col) %>%
    group_by(row) %>%
    mutate(
      colspan=case_when(
        is.na(lead(col)) ~ max.col-col+1,
        TRUE ~ pmax(1,lead(col)-col)
      )
    )


  list(cell_df=cell_df, word_below = words_below)
}


split_multi_cell_cols = function(loc_df, sep_all=FALSE) {
  restore.point("split_mulit_cell_cols")
  if (!"was_split" %in% names(loc_df))
    loc_df$was_split= rep(FALSE, NROW(loc_df))

  loc_df = loc_df %>%
    group_by(row, col) %>%
    mutate(
      num_in_cell = n(),
      in_cell_pos = 1:n(),
      need_sep = num_in_cell > 1 & (sep_all | (type=="num" & is.true(num_deci>0))),
      need_sep_num = ifelse(sum(need_sep)==0, 0,num_in_cell)
    )
  rows = loc_df$num_in_cell > 1
  loc_df$was_split[rows] = TRUE

  col_df = loc_df %>%
    group_by(col) %>%
    summarize(
      sep_num = max(need_sep_num)
    )

  split_cols = sort(col_df$col[col_df$sep_num>1], decreasing=TRUE)

  for (.col in split_cols) {
    rows = loc_df$col > .col
    loc_df$col[rows] = loc_df$col[rows] + 1
    rows = loc_df$col == .col & loc_df$in_cell_pos > 1
    loc_df$col[rows] = loc_df$col[rows] + 1
  }
  if (any(col_df$sep_num>2)) loc_df = split_multi_cell_cols(loc_df, sep_all=sep_all)
  loc_df$max.col = max(loc_df$col)
  loc_df
}

merge_single_neg_int_cols = function(loc_df) {
  #restore.point("merge_single_neg_int_cols")
  # Columns like 2005-2012 in header are sometimes wrongly split
  mod_df = loc_df %>%
    group_by(col) %>%
    mutate(
      num_in_col = n(),
      num_neg_int_in_col = sum(num_deci==0 & num < 0)
    )
  inds = which(mod_df$num_in_col==1 & mod_df$num_neg_int_in_col==1)
  if (length(inds)==0) return(loc_df)

  restore.point("merge_single_neg_int_cols2")
  #stop()
  i = inds[2]
  i = max(inds)
  for (i in rev(inds)) {
    row = loc_df$row[i]
    col = loc_df$col[i]
    left_ind = which(loc_df$row==row & loc_df$col == col-1)
    temp = loc_df[c(left_ind, i),]
    if (!length(left_ind)==1) next
    if (is.true(loc_df$num_deci[left_ind]>0)) next
    loc_df$text[left_ind] = paste0(loc_df$text[left_ind], loc_df$text[i])
    loc_df$type[left_ind] = "text"
    loc_df[left_ind, c("num_str","num","num_deci","paren_type")] = NA
    loc_df$end[left_ind] = loc_df$end[i]
    rows = loc_df$col > col
    loc_df$col[rows] = loc_df$col[rows]-1
    loc_df = loc_df[-i,,drop=FALSE]
  }
  loc_df$max.col = max(loc_df$col)
  loc_df
}

# Unfortunatley the PDF to text converter does not put always
# all cells of a pdf column exactly below each other. Such slips
# may lead to insertion of false additional columns.
# In general it is hard to develop a heuristic to repair such slips
# but in the case we have numbered regression columns (1) (2) (3) (4)
# there is a chance to repair such slips.
# An example is aejapp_3_2_2 Table 1
repair_slipped_cols = function(loc_df) {
  if (!any(loc_df$is_int_header)) return(loc_df)
  restore.point("repair_slipped_cols")

  #if (any(loc_df$num==-16.7)) stop()

  header = loc_df[loc_df$is_int_header,]
  header.row = unique(header$row)
  # We don't know how to deal with multiple int header rows
  if (length(header.row)>1) {
    cat("\nA table seems to have multiple header rows of the form (1) (2) (3) ... We do not yet know how to deal with it for repairing column slips.")
    return(loc_df)
  }


  # We will only be able repair some slips. Consider the following examples:

  # a) Repair feasible
  # (1) | (2) |  | (3)
  #        1        1
  #  2          2   2
  # Here we can move the 2 from the empty column only to the left (2).

  # b) Repeair feasible
  # (1) | (2) |  | (3)
  #  1     1        1
  #  2     2   2
  # Here we can move the 2 from the empty column only to the right (3).

  # c) Repair infeasible
  # (1) |   | (2) | (3)
  #       1         1
  #  2         2    2
  # We cannot say whether the empty column shall be moved left or right

  header = header %>%
    arrange(col)

  col_diffs = diff(header$col)
  if (!setequal(col_diffs, 1:2)) return(loc_df)
  move_cols = setdiff(unique(loc_df$col), header$col)
  move_cols = move_cols[move_cols>min(header$col) & move_cols<max(header$col)]



  loc_df = loc_df %>%
    arrange(row, col) %>%
    group_by(row) %>%
    mutate(
      is_above = row <= header.row,
      left_free =  is.true( (col-lag(col)>=2) | (is.na(lag(col) & col > 1))),
      right_free = !is.true(!lead(left_free))
    )

  #test = filter(loc_df, col == 8)

  col_df = loc_df %>%
    group_by(col) %>%
    summarize(
      want_move = first(col %in% move_cols),
      move_left = want_move & all(left_free[!is_above]) & !(all(right_free[!is_above])),
      move_right = want_move & !all(left_free[!is_above]) & (all(right_free[!is_above])),
      do_move = want_move & (move_left + move_right == 1)
    )

  # We need at least one move col we want to move
  if (!any(col_df$do_move)) {
    return(loc_df)
  }

  loc_df = left_join(loc_df, col_df, by="col")

  # Perform left moves
  rows = loc_df$do_move & loc_df$move_left & loc_df$row > header.row
  loc_df$col[rows] = loc_df$col[rows]-1L

  # Perform right moves
  rows = loc_df$do_move & loc_df$move_right & loc_df$row > header.row
  loc_df$col[rows] = loc_df$col[rows]+1L

  # Remove gaps from move_cols
  for (i in rev(sort(move_cols))) {
    rows = loc_df$col >= i & loc_df$row >= header.row
    loc_df$col[rows] = loc_df$col[rows]-1
    # Rows above the header row will only be moved if the left col is free
    # We may miss some movements if left is free only after the earlier col
    # is moved
    # Once we have a concrete example that could be improved
    # we again adapt the code
    rows = loc_df$col >= i & loc_df$row < header.row & loc_df$left_free
    loc_df$col[rows] = loc_df$col[rows]-1
  }

  # We call again if not all wanted cols could be moved
  # E.g. relevant here (see aejapp_3_2_2 Table 1, lower panel):
  # (1) |   | (2) |   | (3)
  #       1         1    1
  #       2         2    2
  # In the first iteration we can move the last gap to (2)
  # if we then call again, we can move also the first gap to (1)
  if (any(col_df$want_move & !col_df$do_move)) {
    loc_df = repair_slipped_cols(loc_df)
  }
  loc_df
}

repair_same_col_cells = function(loc_df) {
  restore.point("repair_same_col_cells")
  #return(loc_df)
  org_loc_df = loc_df

  #if (any(loc_df$num==-16.7)) stop()

  loc_df = loc_df %>%
    group_by(line) %>%
    mutate(
      max_col = max(col),
      lag_col = lag(col),
      lead_col = lead(col)
    ) %>%
    ungroup() %>%
    mutate(
      same_col_as_left = is.true(lag_col==col),
      same_col_as_right = is.true(lead_col==col)
    )
  if (!any(loc_df$same_col_as_left)) {
    return(org_loc_df)
  }

  max_col = max(loc_df$col)
  rows = which(loc_df$same_col_as_left | loc_df$same_col_as_right)
  lo_df = loc_df[rows,]
  lo_df = lo_df %>%
    group_by(line) %>%
    mutate(
      might_move_left = same_col_as_right & type == "num" & !is.true(lag_col>=col-1) & is.true(lag_col >= 1),
      might_move_right = same_col_as_left & type == "num" & !is.true(lead_col>=col-1) & is.true(lead_col <= max_col)
    ) %>%
    mutate(
      move_left = might_move_left & !(lead(might_move_right)),
      move_right = might_move_right & !(lag(might_move_left))
    )

  move.left.rows = rows[lo_df$move_left]
  org_loc_df$col[move.left.rows] = org_loc_df$col[move.left.rows]-1

  move.right.rows = rows[lo_df$move_right]
  org_loc_df$col[move.right.rows] = org_loc_df$col[move.right.rows]+1

  return(org_loc_df)
}


find_stars_str = function(big_str) {
  restore.point("find_stars_str")
  if (length(big_str)==0) return(NULL)
  digit_pos = stringi::stri_locate_last_regex(big_str, "[0-9]")[,1]
  digit_pos[is.na(digit_pos)] = 0
  right_str =  substring(big_str,digit_pos+1)
  right_str = gsub(" ", "", right_str)
  as.vector(stringi::stri_match_first_regex(right_str, "[*]+")) %>% na.val("")
}


scitab_num_rx = function() {
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
  re.big
}


line_df_find_table_parts = function(line_df, tab_df) {
  restore.point("line_df_find_table_parts")
  type = line_df$type
  tabid = line_df$tabid
  tab_row = 2

  for (tab_row in seq_len(NROW(tab_df))) {
    tab = tab_df[tab_row,]
    type[tab$title_line] = "tabtitle"
    note_lines = extract_tab_note_lines(line_df$txt, line_df$trim_txt, tab$end_line, line_df$rev_pline[tab$end_line])
    if (!is.null(note_lines)) {
      type[from_to(tab$start_line+1,note_lines[1]-1)] = "tab"
      type[from_to(note_lines[1], note_lines[2])] = "tabnotes"
      tabid[tab$start_line:note_lines[2]] = tab$tabid
    } else {
      type[from_to(tab$start_line+1,tab$end_line)] = "tab"
      tabid[tab$start_line:tab$end_line] = tab$tabid
    }
  }
  line_df$type = type
  line_df$tabid = tabid
  line_df
}


extract_tab_note_lines = function(txt,trim_txt, tab_end_line, rev_pline) {
  restore.point("extract_tab_note_lines")
  n = length(txt)
  lines = from_to(tab_end_line+1,tab_end_line+min(10, rev_pline-1), max=n)
  lines = lines[startsWith(trim_txt[lines],"Note")]
  if (length(lines)==0) return(NULL)

  line = first(lines)
  is_ok = is_really_a_note_line(txt[line], trim_txt[line])
  if (!is_ok) return(NULL)


  note_start_line = line
  remaining_plines = rev_pline - (note_start_line - tab_end_line) -1

  lines = from_to(note_start_line+1,tab_end_line+remaining_plines, max=n)
  str = c(trim_txt[lines],"")
  empty = min(which(str==""))

  c(note_start_line=note_start_line, note_end_line=note_start_line + empty-1)
}


is_really_a_note_line = function(txt, trim_txt) {
  restore.point("is_really_a_note_line")

  if (stri_detect_regex(trim_txt,"^Note[s]?[\\:\\.]")) return(TRUE)

  words = stringi::stri_extract_all_words(trim_txt, omit_no_match=TRUE)[[1]]
  if(length(words)<=1) return(TRUE)
  # That should be sufficients since no normal
  # sentence should start with the word "Notes"
  if (words[1]=="Notes") return(TRUE)
  # If the second word starts with a capital letter
  # we likely have a TRUE note
  if (words[2]!=tolower(words[2])) return(TRUE)

  # The expression "Note that" suggests a normal sentence
  if (words[2]=="that") return(FALSE)

  if (startsWith(txt,"      ")) return(TRUE)
  #stop()
  return(FALSE)
}


# Returns a tibble like
# than tab_from_html.Rds from a HTML file
line_df_extract_table_parts = function(line_df) {
  tabids = unique(line_df$tabid) %>% setdiff("")

  tabid = 1
  tab_parts = lapply(tabids, function(tabid) {
    ldf = line_df[is.true(line_df$tabid==tabid),]
    start_page = first(ldf$page)
    tabtitle = ldf$trim_txt[ldf$type=="tabtitle"]
    tabsource = merge.lines(ldf$txt[ldf$type=="tab"])
    tabnotes = combine_text_lines(ldf$trim_txt[ldf$type=="tabnotes"])

    tibble(tabid=tabid, tab_counter = NA_integer_, tabtitle=tabtitle, tabnotes=tabnotes, cell_df = list(NULL), tabsource = tabsource, nchar_title = nchar(tabtitle), nchar_tabsource=nchar(tabsource), nchar_notes = nchar(tabnotes), start_page=start_page)
  }) %>% bind_rows()
  tab_parts$tab_counter = seq_rows(tab_parts)
  tab_parts
}

pad_tabid = function(tabid) {
  #num = as.numeric(stri_extract_first_regex(tabid, "[0-9]+[.0-9]*"))
  #prefix = stri_extract_first_regex(tabid, "^[a-zA-Z]") %>% na.val("")
  stringi::stri_pad_left(tabid, width=max(nchar(tabid)),pad = "0")

}


