


plot_iycf_areagraph <- function(df) {
  print("Position 1")

  if(!(c("age_months") %in% names(df))) {
    print("Position 2")
    stop("Age in months of the child is required to create the IYCF Area Graph. age_months column is not in your data. Please check your input.")

  } else {
    print("Position 3")
    df <- df %>%
      dplyr::filter(!is.na(age_months)) %>% dplyr::filter(age_months < 24) %>%
      mutate(age_group = ifelse(age_months <= 1, "0-1 months",
                                ifelse(age_months <=3, "2-3 months",
                                       ifelse(age_months <= 5, "4-5 months",
                                              ifelse(age_months <= 7, "6-7 months",
                                                     ifelse(age_months <= 9, "8-9 months",
                                                            ifelse(age_months <= 11, "10-11 months",
                                                                   ifelse(age_months <= 13, "12-13 months",
                                                                          ifelse(age_months <=15, "14-15 months",
                                                                                 ifelse(age_months <= 17, "16-17 months",
                                                                                        ifelse(age_months <= 19, "18-19 months",
                                                                                               ifelse(age_months <=21, "20-21 months",
                                                                                                      ifelse(age_months <=23, "22-23 months", "")))))))))))))

    area_graph_vars <- c("iycf_ebf", "iycf_4", "iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j",
                         "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")

    if(length(setdiff(area_graph_vars, names(df)))==0) {

    } else {stop("You don't have the required variables to create the IYCF Area Graph. Please check your input. You require at least ")}

    food_vars <- c("iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")


    df <- df %>%
      mutate(any_food = ifelse((iycf_7a == 1 | iycf_7b == 1 | iycf_7c == 1 | iycf_7d == 1 | iycf_7e == 1 | iycf_7f == 1 | iycf_7g == 1 | iycf_7h == 1 | iycf_7i == 1 | iycf_7j == 1 | iycf_7k == 1 | iycf_7l == 1 | iycf_7m == 1 | iycf_7n == 1 | iycf_7o == 1 | iycf_7p == 1 | iycf_7q == 1 | iycf_7r == 1), 1, 0),
             any_food = ifelse(is.na(any_food), 0, any_food),
             no_food = ifelse(iycf_7a != 1 & iycf_7b != 1 & iycf_7c != 1 & iycf_7d != 1 & iycf_7e != 1 & iycf_7f != 1 & iycf_7g != 1 & iycf_7h != 1 & iycf_7i != 1 & iycf_7j != 1 & iycf_7k != 1 & iycf_7l != 1 & iycf_7m != 1 & iycf_7n != 1 & iycf_7o != 1 & iycf_7p != 1 & iycf_7q != 1 & iycf_7r != 1, 1, 0),
             no_food = ifelse(is.na(no_food), 0, no_food),
             no_liquid = ifelse(iycf_6b != 1 & iycf_6c != 1 & iycf_6d != 1 & iycf_6b != 1 & iycf_6e != 1 & iycf_6f != 1 & iycf_6g != 1 & iycf_6h != 1 & iycf_6i != 1 & iycf_6j != 1, 1, 0),
             no_liquid = ifelse(is.na(no_liquid), 0, no_liquid),
             bf = ifelse(iycf_4 == 1, 1, 0),
             bf = ifelse(is.na(bf), 0, bf),
             )


    df <- df %>%
      mutate(category = case_when(

        bf == 0 ~ "Not Breastfed",
        bf == 1 & any_food == 1  ~ "Breastfed & Solid or Semi-Solid Foods",
        bf == 1 & (iycf_6b == 1 | iycf_6c == 1 | iycf_6d == 1 | iycf_7a == 1) & no_food == 1  ~ "Breastfed & Animal Milk or Formula",
        bf == 1 & no_food == 1 & (iycf_6b != 1 & iycf_6c != 1 & iycf_6d != 1 & iycf_7a != 1) & (iycf_6e == 1 | iycf_6f == 1 | iycf_6g == 1 | iycf_6h == 1 | iycf_6i == 1 | iycf_6j == 1) ~ "Breastfed & Non-Milk Liquids",
        bf == 1 & no_food == 1 & no_liquid == 1 & iycf_6a == 1 ~ "Breasfed & Plain Water",
        bf == 1 & no_food == 1 & no_liquid == 1 & iycf_6a != 1 ~ "Exclusive Breastfed",
        TRUE ~ "Unknown",
      ))

    # df <- df %>%
    #   mutate(category = ifelse(iycf_ebf == 1, "Exclusive Breastfed",
    #                            ifelse(iycf_4 == 1 & iycf_6a == 1 & iycf_6b == 2 & iycf_6c == 2 & iycf_6d == 2 & iycf_6e == 2 & iycf_6f == 2 & iycf_6g == 2 & iycf_6h == 2 & iycf_6i == 2 & iycf_6j == 2, "Breasfed & Plain Water",
    #                                   ifelse(iycf_4 == 1 & ( iycf_6a == 1 | iycf_6b != 1 | iycf_6e != 1 & iycf_6f != 1 & iycf_6g != 1 & iycf_6h != 1 & iycf_6i != 1 & iycf_6j != 1), "Breastfed & Non-Milk Liquids",
    #                                          ifelse(iycf_4 == 1 & (iycf_6b == 1 | iycf_6c == 1 | iycf_6d == 1), "Breastfed & Animal Milk or Formula",
    #                                                 ifelse(iycf_4 == 1 & (iycf_7a == 1 | iycf_7b == 1 | iycf_7c == 1 | iycf_7d == 1 | iycf_7e == 1 | iycf_7f == 1 | iycf_7g == 1 | iycf_7h == 1 | iycf_7i == 1 | iycf_7j == 1 | iycf_7k == 1 | iycf_7l == 1 | iycf_7m == 1 | iycf_7n == 1 | iycf_7o == 1 | iycf_7p == 1 | iycf_7q == 1 | iycf_7r == 1), "Breastfed & Solid or Semi-Solid Foods",
    #                                                        ifelse(iycf_4 != 1, "Not Breastfed", NA)))))))
    #
    print("Position 4")
    print(head(df))

    df <- df %>%
      dplyr::group_by(category, age_group) %>%
      dplyr::summarize(n = sum(!is.na(iycf_4))) %>%
      ungroup() %>%
      group_by(age_group) %>%
      dplyr::mutate(percentage = (n / sum(n))*100,
                    n = NULL) %>%
      dplyr::filter(!is.na(category)) %>%
      arrange(percentage)

    print("Position 5")

    df <- merge(df,
                expand.grid(age_group=unique(df$age_group),
                            category=unique(df$category),
                            stringsAsFactors=F),
                all.y=T)

    # Fill NA values with zeros
    df$percentage[is.na(df$percentage)] <- 0

    df <- df %>% arrange(percentage) %>% mutate(age_group = factor(age_group, levels = c("0-1 months", "2-3 months", "4-5 months", "6-7 months", "8-9 months", "10-11 months", "12-13 months", "14-15 months", "16-17 months", "18-19 months", "20-21 months", "22-23 months")),
                                                category = factor(category, levels = c("Unknown", "Not Breastfed", "Breastfed & Solid or Semi-Solid Foods", "Breastfed & Animal Milk or Formula", "Breastfed & Non-Milk Liquids", "Breasfed & Plain Water", "Exclusive Breastfed")))

    print(df)

    mybrewerpal <- function(n, name) {# modified RcolorBrewer::brewer.pal
      ## the first call to switch would not be necessary in this example,
      ## but I leave it in order to make the underlying structure in brewer.pal clearer
      switch(name, mypal = switch(n - 2, rgb(c(255, 254, 217), c(247, 196, 95), c(188, 79, 14), maxColorValue = 255),
                                  rgb(c(255, 254, 254, 204), c(255, 217, 153, 76), c(212, 142, 41, 2), maxColorValue = 255),
                                  rgb(c(255, 254, 254, 217, 153), c(255, 217, 153, 95, 52), c(212, 142, 41, 14, 4), maxColorValue = 255),
                                  rgb(c(255, 254, 254, 254, 217, 153), c(255, 227, 196, 153, 95, 52), c(212, 145, 79, 41, 14, 4), maxColorValue = 255),
                                  rgb(c(255, 254, 254, 254, 236, 204, 140), c(255, 227, 196, 153, 112, 76, 45), c(212, 145, 79, 41, 20, 2, 4), maxColorValue = 255),
                                  rgb(c(255, 255, 254, 254, 254, 236, 204, 140), c(255, 247, 227, 196, 153, 112, 76, 45), c(229, 188, 145, 79, 41, 20, 2, 4), maxColorValue = 255),
                                  rgb(c(255, 255, 254, 254, 254, 236, 204, 153, 102), c(255, 247, 227, 196, 153, 112, 76, 52, 37), c(229, 188, 145, 79, 41, 20, 2, 4, 6), maxColorValue = 255)
      ))
    }

    brewer_pal2 <- # modified from scales:::brewer_pal
      function() { # stripped down all arguments, just to show the core
        function(n) {
          mybrewerpal(n, "mypal") ##modified, usually this is selected by a function
          ## with type and name as arguments, selecting a palette from a list called scales:::brewer
        }
      }

    scale_fill_custom <- ### modified from scale_fill_brewer, removed some arguments
      function (..., aesthetics = "fill") {
        discrete_scale(aesthetics, "custom", brewer_pal2(), ...) ## give a new name to the
        ## scale, it will create a new Scale object.
      }

    g <- ggplot(data = df, aes(x = age_group, y = percentage)) +
      geom_area(aes(fill = category, group = category), alpha=1 , size=0.3, colour="black", position = "stack") +
      scale_fill_custom() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


    # return(g)



  }

  return(g)

}
