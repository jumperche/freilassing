# freilassing
### README for GitHub Repository

# Shiny Application for Events and Articles Overview

This Shiny application combines articles and event data from various sources, providing a comprehensive and user-friendly interface. Designed with the `shinyMobile` package, the app is mobile-first, offering seamless functionality across devices. It fetches, processes, and dynamically displays information in real-time, with options to update and manage data efficiently.

---

## Features

- **Responsive Design**: Built with `shinyMobile`, ensuring compatibility with mobile and desktop devices.
- **Data Integration**: Combines data from multiple sources, including:
  - Freilassing website (articles and events)
  - VDK website (articles and events)
  - VHS courses
- **Automated Updates**: Automatically updates articles and events at fixed intervals or upon user request.
- **Popup Details**: Display detailed views of articles and events using a modern popup interface.
- **Local Storage**: Saves fetched data locally for offline access and faster reload times.
- **Customizable UI**: Stylish cards and layouts for displaying content with or without images.

---

## Prerequisites

Before running the app, ensure you have the following installed:

- R (version ≥ 4.0)
- RStudio (optional)
- Required R packages:
  ```r
  install.packages(c(
    "shiny", "shinyMobile", "rvest", "dplyr", "stringr", "lubridate",
    "shinyjs", "shinyalert", "furrr", "promises", "future", "shinyWidgets"
  ))
  ```

---

## Getting Started

1. **Clone the Repository**
   ```bash
   git clone <repository-url>
   cd <repository-directory>
   ```

2. **Launch the App**
   Open `app.R` in RStudio or your preferred R IDE, and click "Run App". Alternatively, run:
   ```r
   shiny::runApp("path/to/app.R")
   ```

3. **Navigate the Interface**
   - Use the "Articles" tab to view and explore the latest articles.
   - Use the "Events" tab to see upcoming events, including details and registration options.
   - Access the "Settings" tab for customizable options (future enhancements).

---

## Folder Structure

```plaintext
.
├── app.R               # Main application file
├── articles_data.rds   # Local storage for article data
├── events_data.rds     # Local storage for event data
└── README.md           # This README file
```

---

## Key Functions and Code Highlights

### Articles

- `get_freilassing_articles()`: Fetches articles from Freilassing's website.
- `get_vdk_articles()`: Fetches articles from VDK's website.
- `combine_articles()`: Combines and deduplicates articles from all sources.

### Events

- `get_freilassing_events()`: Scrapes event data from Freilassing's event page.
- `get_vdk_events()`: Retrieves event data from VDK.
- `get_vhs_events()`: Scrapes course data from VHS categories.
- `combine_events()`: Merges events from all sources, filters past events, and sorts them by date.

### UI Features

- Uses `f7TabLayout` for smooth navigation.
- Custom CSS enhances the visual appeal of cards and buttons.
- Popups are created with `shinyalert` for detailed content views.

---

## Future Enhancements

- **Search and Filter**: Allow users to search and filter articles/events.
- **Notifications**: Push notifications for new articles or events.
- **User Authentication**: Login support for personalized features.

---

## Contribution

Feel free to contribute to this project by submitting issues or pull requests. Ensure that your code follows proper guidelines and is thoroughly tested.

---

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

---

Start exploring the articles and events seamlessly with this Shiny application! 😊
