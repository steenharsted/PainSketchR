# Function to calculate the simple hip flexion angle in the movement plane
calculate_hip_flexion_in_movement_plane_simple <- function(hip_x, hip_y, knee_x, knee_y) {
  vector_x <- knee_x - hip_x
  vector_y <- knee_y - hip_y
  hypotenuse <- sqrt(vector_x^2 + vector_y^2)
  
  if (hypotenuse == 0) {
    return(0)
  } else {
    sin_angle <- asin(vector_x / hypotenuse) * (180 / pi)
    return(sin_angle)
  }
}



# Function to add hip flexion angles for right and left hips in the dataframe
add_hip_flexion_in_movement_plane_simple <- function(data, 
                                                     right_hip_x_col = RH_MPF, 
                                                     right_hip_y_col = RH_MPU, 
                                                     right_knee_x_col = RK_MPF,
                                                     right_knee_y_col = RK_MPU, 
                                                     left_hip_x_col = LH_MPF,
                                                     left_hip_y_col = LH_MPU, 
                                                     left_knee_x_col = LK_MPF, 
                                                     left_knee_y_col = LK_MPU) {
  data %>%
    rowwise() %>%
    mutate(
      RHF_simple_MP = calculate_hip_flexion_in_movement_plane_simple({{ right_hip_x_col }}, {{ right_hip_y_col }}, {{ right_knee_x_col }}, {{ right_knee_y_col }}),
      LHF_simple_MP = calculate_hip_flexion_in_movement_plane_simple({{ left_hip_x_col }}, {{ left_hip_y_col }}, {{ left_knee_x_col }}, {{ left_knee_y_col }})
    ) %>%
    ungroup()
}



# Usage example assuming 'data' has columns for right and left hip and knee positions
# Replace 'RH_MPF', 'RH_MPU', etc. with the actual column names from your dataset
# sample_synt_data %>% 
#   mutate(
#     LH_MPF = RH_MPF,
#     LH_MPU = RH_MPU,
#     LK_MPF = RK_MPF,
#     LK_MPU = RK_MPU
#   ) %>% 
#   
#   add_hip_flexion_in_movement_plane_simple()




# Calculate forward lean
calculate_forward_lean <- function(avg_hip_x, avg_hip_y, avg_shoulder_x, avg_shoulder_y) {
  vector_x <- avg_shoulder_x - avg_hip_x
  vector_y <- avg_shoulder_y - avg_hip_y
  hypotenuse <- sqrt(vector_x^2 + vector_y^2)
  
  if (hypotenuse == 0) {
    return(0)
  } else {
    cos_angle <- acos(vector_y / hypotenuse) * (180 / pi)
    # Adjust the angle based on the position of the shoulder relative to the hip
    if (avg_shoulder_x < avg_hip_x) {
      cos_angle <- -cos_angle
    }
    return(cos_angle)
  }
}

# Function to add forward lean angle to the dataframe
add_forward_lean <- function(data, 
                                                     hip_x_col = avg_hip_MPF, 
                                                     hip_y_col = avg_hip_MPU, 
                                                     shoulder_x_col = avg_shoulder_MPF, 
                                                     shoulder_y_col = avg_shoulder_MPU) {
  data %>%
    rowwise() %>%
    mutate(
      forward_lean = calculate_forward_lean ({{ hip_x_col }}, {{ hip_y_col }}, {{ shoulder_x_col }}, {{ shoulder_y_col }})
    ) %>%
    ungroup()
}


# Calculate side lean
calculate_side_lean <- function(avg_hip_z, avg_hip_y, avg_shoulder_z, avg_shoulder_y) {
  vector_z <- avg_shoulder_z - avg_hip_z
  vector_y <- avg_shoulder_y - avg_hip_y
  hypotenuse <- sqrt(vector_z^2 + vector_y^2)
  
  if (hypotenuse == 0) {
    return(0)
  } else {
    cos_angle <- acos(vector_y / hypotenuse) * (180 / pi)
    # Adjust the angle based on the position of the shoulder relative to the hip
    if (avg_shoulder_z < avg_hip_z) {
      cos_angle <- -cos_angle
    }
    return(cos_angle)
  }
}

# Function to add side lean angle to the dataframe
add_side_lean <- function(data, 
                             hip_z_col = avg_hip_MPR, 
                             hip_y_col = avg_hip_MPU, 
                             shoulder_z_col = avg_shoulder_MPR, 
                             shoulder_y_col = avg_shoulder_MPU) {
  data %>%
    rowwise() %>%
    mutate(
      side_lean = calculate_side_lean ({{ hip_z_col }}, {{ hip_y_col }}, {{ shoulder_z_col }}, {{ shoulder_y_col }})
    ) %>%
    ungroup()
}


# Calculate shoulder tilt 
calculate_shoulder_tilt <- function(right_shoulder_z, right_shoulder_y,
                                    left_shoulder_z, left_shoulder_y) {
  dz <- right_shoulder_z - left_shoulder_z
  dy <- right_shoulder_y - left_shoulder_y
  
  if (dz == 0 && dy == 0) {
    return(0)
  }
  
  angle_rad <- atan2(dy, dz)
  angle_deg <- angle_rad * 180 / pi
  
  return(angle_deg)
}

# Function to add side lean angle to the dataframe
add_shoulder_tilt <- function(data, 
                              r_shoulder_z_col = RSZ,
                              r_shoulder_y_col = RSY,
                              l_shoulder_z_col = LSZ,
                              l_shoulder_y_col = LSY) {
  data %>%
    rowwise() %>%
    mutate(
      shoulder_tilt = calculate_shoulder_tilt(
        {{ r_shoulder_z_col }}, {{ r_shoulder_y_col }},
        {{ l_shoulder_z_col }}, {{ l_shoulder_y_col }}
      )
    ) %>%
    ungroup()
}



# Usage example
# sample_synt_data %>% 
#   mutate(
#     LH_MPF = RH_MPF,
#     LH_MPU = RH_MPU,
#     LK_MPF = RK_MPF,
#     LK_MPU = RK_MPU,
#     avg_hip_MPF = (LH_MPF + RH_MPF)/2,
#     avg_hip_MPU = (LH_MPU + RH_MPU)/2
#   ) %>% 
#   add_hip_flexion_in_movement_plane_simple() %>% 
#   add_forward_lean_in_movment_plane_simple() %>% 
#   mutate(
#     RHF_hard_MP = RHF_simple_MP + forward_lean_simple,
#     LHF_hard_MP = LHF_simple_MP + forward_lean_simple) 
#   

# Function to remove empty 
remove_empty <- function(x) x[x != ""]

