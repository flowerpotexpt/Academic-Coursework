# -*- coding: utf-8 -*-
"""Mountain Run Q Learning 26.03.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1JJWhKwoO2RzjggPwCirmdnMcsjK9A8GK
"""

# > /dev/null 2>&1 silences output
!pip install box2d-py #> /dev/null 2>&1
!pip install gym pyvirtualdisplay
!apt-get install -y xvfb python-opengl ffmpeg
!apt-get install x11-utils
!apt-get install python-opengl
!apt-get install xvfb

#!pip install pyvirtualdisplay



# Commented out IPython magic to ensure Python compatibility.
# import necessary packages, install some if needed
import gym
from gym import logger as gymlogger
from gym.wrappers import Monitor
gymlogger.set_level(40) #error only
from google.colab import files
from datetime import datetime
import time
from tqdm import tqdm

import numpy as np
import matplotlib.pyplot as plt
import math
import glob
import io
import base64
from IPython.display import HTML


#eg screen resolution 1400x900
from pyvirtualdisplay import Display
display = Display(visible=0, size=(1400, 900))
display.start()

# Custom functions for visualization and creating environments in colab 
# https://star-ai.github.io/Rendering-OpenAi-Gym-in-Colaboratory/?fbclid=IwAR2y2gSNQv6tQag9JorgnG-O3J5Rx-QJLqK9lbEv9MSZcdt-YroXFzv0VU4

import gym
from gym import logger as gymlogger
from gym.wrappers import Monitor
gymlogger.set_level(40) #error only
import tensorflow as tf
import numpy as np
import random
import matplotlib
import matplotlib.pyplot as plt
# %matplotlib inline
import math
import glob
import io
import base64
from IPython.display import HTML

from IPython import display as ipythondisplay
from pyvirtualdisplay import Display
display = Display(visible=0, size=(1400, 900))
display.start()

def show_video():
  mp4list = glob.glob('video/*.mp4')
  if len(mp4list) > 0:
    mp4 = mp4list[0]
    video = io.open(mp4, 'r+b').read()
    encoded = base64.b64encode(video)
    ipythondisplay.display(HTML(data='''<video alt="test" autoplay 
                loop controls style="height: 400px;">
                <source src="data:video/mp4;base64,{0}" type="video/mp4" />
             </video>'''.format(encoded.decode('ascii'))))
  else: 
    print("Could not find video")
    

def wrap_env(env):
  env = Monitor(env, './video', force=True)
  return env

# mount colab to google drive, access to write and read files
# needs authorization first, go to url and enter code

#If not yet mounted, uncomment these
from google.colab import drive
drive.mount('/content/gdrive)



# Hyperparameters and other definitions

# Get folder path to save files inside drive
!ls "/content/gdrive/My Drive/Colab Notebooks"
root_dir = "/content/gdrive/My Drive/Colab Notebooks/"
drive_path = root_dir + "OW_ML/"
model_name = "OW_ML"

# Import a model to expand training
USE_PREV_MODEL = False
if USE_PREV_MODEL:
  root_dir = "/content/gdrive/My Drive/Colab Notebooks/"
  model_path = root_dir + "PT202004/PT202004episode30000-qtable.npy"
  q_table = np.load(model_path)

LEARNING_RATE = 0.2
DISCOUNT = 0.995
EPISODES = 1_000

SAVE_RESULTS = True
SHOW_EVERY = 9900
STATS_EVERY = 100
ep_rewards = []
changed_eps= {'ep': [], 'eps': []}
aggr_ep_rewards = {'ep': [], 'avg': [], 'max': [], 'min': []}

# Create and reset game environment
env = wrap_env(gym.make("MountainCar-v0"))
#print(env.observation_space.high, env.observation_space.low)

# Discretize the states, so we don't have an enormous continuous q table
DISCRETE_OS_SIZE = [40] * len(env.observation_space.high)
discrete_os_win_size = (env.observation_space.high - env.observation_space.low)/DISCRETE_OS_SIZE
#print(discrete_os_win_size)

# function to get the transformed discretized state from game states
def get_discrete_state(state):
  discrete_state = (state - env.observation_space.low)/discrete_os_win_size
  return tuple(discrete_state.astype(np.int))

if not USE_PREV_MODEL:
  # Initialize q table, random numbers
  q_table = np.random.uniform(low=-2, high=0, size=(DISCRETE_OS_SIZE + [env.action_space.n]))  #np.shape(q_table)
  # Formula for updating q values : new_q = (1 - LEARNING_RATE) * current_q + LEARNING_RATE * (reward + DISCOUNT * max_future_q)
  print("q table initialized at:", datetime.now().strftime("%d_%m_%Y.%Hh%M"))

# Exploration settings, decide whether to use learned q table (exploitation) or randomize movement (exploration)
epsilon = 1  # not a constant, qoing to be decayed
epsilon_decay_value = 0.999
min_epsilon = 0.01
#START_EPSILON_DECAYING = 1
#END_EPSILON_DECAYING = EPISODES//2
#epsilon_decay_value = epsilon/(END_EPSILON_DECAYING - START_EPSILON_DECAYING)
#print(END_EPSILON_DECAYING, epsilon_decay_value, DISCOUNT, LEARNING_RATE)



import time
start_time = time.time()
run_start = datetime.now().strftime("%d_%m_%Y.%Hh%M")

for episode in tqdm(range(1, EPISODES + 1)):
    episode_reward = 0
    discrete_state = get_discrete_state(env.reset())
    done = False
    
    # leave in for visualization, not working in colab
    if episode % SHOW_EVERY == 0:
        render = True
    else:
        render = False

    # Run game until done
    while not done:
      # Decide whether to explore or exploit, epsilon decays as episodes progress, more likely to exploit
      if np.random.random() > epsilon:
            # Get action from Q table
            action = np.argmax(q_table[discrete_state])
      else:
            # Get random action
            action = np.random.randint(0, env.action_space.n)
      # Update game states
      new_state, reward, done, _ = env.step(action)
      new_discrete_state = get_discrete_state(new_state)
      episode_reward += reward
      
      if episode % SHOW_EVERY == 0:
            env.render()

      # If simulation did not end yet after last step - update Q table
      if not done:
        # Maximum possible Q value in next step (for new state)
        max_future_q = np.max(q_table[new_discrete_state])

        # Current Q value (for current state and performed action)
        current_q = q_table[discrete_state + (action,)]

        # And here's our equation for a new Q value for current state and action
        new_q = (1 - LEARNING_RATE) * current_q + LEARNING_RATE * (reward + DISCOUNT * max_future_q)

        # Update Q table with new Q value
        q_table[discrete_state + (action,)] = new_q

      # Simulation ended (for any reason) - if goal position is achived - update Q value with reward directly
      elif new_state[0] >= env.goal_position:
        #q_table[discrete_state + (action,)] = 0
        #q_table[discrete_state + (action,)] = reward
        q_table[discrete_state + (action,)] = 1000


      discrete_state = new_discrete_state

    changed_eps['ep'].append(episode)
    changed_eps['eps'].append(epsilon)
    epsilon = max(min_epsilon, epsilon * epsilon_decay_value)
    #if END_EPSILON_DECAYING >= episode >= START_EPSILON_DECAYING:
    #  epsilon -= epsilon_decay_value

    #Append reward for statistics
    ep_rewards.append(episode_reward) 
    if not episode % STATS_EVERY:
      average_reward = sum(ep_rewards[-STATS_EVERY:])/STATS_EVERY
      aggr_ep_rewards['ep'].append(episode)
      aggr_ep_rewards['avg'].append(average_reward)
      aggr_ep_rewards['max'].append(max(ep_rewards[-STATS_EVERY:]))
      aggr_ep_rewards['min'].append(min(ep_rewards[-STATS_EVERY:]))
      print(f'Episode: {episode:>5d}, average reward: {average_reward:>4.1f}, current epsilon: {epsilon:>1.2f}')

    if episode % 100 == 0 and SAVE_RESULTS:
      model_save_name = f"{model_name}episode{episode}-qtable.npy"
      path = drive_path + model_save_name
      np.save(path, q_table)

env.close()

time_elapsed = time.time() - start_time
print("time elapsed: {:.2f}s".format(time_elapsed))

# Plot reward statistics over episodes
stats_plot =plt.figure()

plt.plot(aggr_ep_rewards['ep'], aggr_ep_rewards['avg'], label="average rewards")
plt.plot(aggr_ep_rewards['ep'], aggr_ep_rewards['max'], label="max rewards")
plt.plot(aggr_ep_rewards['ep'], aggr_ep_rewards['min'], label="min rewards")

plt.legend(loc=2)
plt.ylim(-210, -50)
plt.grid(True)
plt.show()

if SAVE_RESULTS:
  stats_plot.show()
  plot_save_name = f"mountain_car_run{model_name}_stats.png"
  plot_path = drive_path + plot_save_name
  stats_plot.savefig(plot_path, dpi=(250))
  #files.download(plot_path) 
  for key in aggr_ep_rewards.keys():
    stats_array = np.array(aggr_ep_rewards[key])
    stats_save_name = f"{model_name}-stats-{key}.npy"
    stats_path = drive_path + stats_save_name
    np.save(stats_path, stats_array)

  eps_plot = plt.figure()
  plt.plot(changed_eps["ep"], changed_eps["eps"], label="epsilon over episodes")
  plt.legend(loc=1)
  eps_plot.show()
  eps_plot_save_name = f"mountain_car_run{model_name}_eps.png"
  plot_path = drive_path + eps_plot_save_name
  eps_plot.savefig(plot_path, dpi=(250))
  
  for key in changed_eps.keys():
    stats_array = np.array(changed_eps[key])
    stats_save_name = f"{model_name}-epsilon-{key}.npy"
    stats_path = drive_path + stats_save_name
    np.save(stats_path, stats_array)







# Load a model and check how it performs
load_previous_model = False
if load_previous_model:
  root_dir = "/content/gdrive/My Drive/Colab Notebooks/"
  model_path = root_dir + "Mountain run 4/Mountain run 4_a0.5_23_03_2020.23h57-episode10000-qtable.npy"
  q_table = np.load(model_path)

env = wrap_env(gym.make("MountainCar-v0"))

EPISODES = 1
for episode in range(EPISODES):
    episode_reward = 0
    discrete_state = get_discrete_state(env.reset())
    done = False
    render = True

    # Run game until done
    while not done:
      # Decide whether to explore or exploit, epsilon decays as episodes progress, more likely to exploit
      action = np.argmax(q_table[discrete_state])
      # Update game states
      new_state, reward, done, _ = env.step(action)
      new_discrete_state = get_discrete_state(new_state)
      env.render()
      discrete_state = new_discrete_state

env.close()
show_video()

import gym
import os
import random
import numpy as np
from statistics import mean


class MountainCar:
    def __init__(self, epsilon, alpha, gamma):
        self.system = "OS X"
        self.env = wrap_env(gym.make('MountainCar-v0'))
        self.action_space = self.env.action_space.n  # 0 is push left, 1 is  no push and 2 is push right
        self.observation_space = self.env.observation_space  # 0 is position [-1.2 - 0.6], 1 is velocity [-0.07 - 0.07]

        self.epsilon = epsilon  # probability of choosing a random action
        self.alpha = alpha  # learning rate
        self.gamma = gamma  # discount rate

        self.velocity_obs = [i / 100 for i in range(-7, 8, 1)]
        self.pos_obs = [i / 10 for i in range(-12, 7, 1)]
        obs_size = len(self.velocity_obs) * len(self.pos_obs)
        self.path = "/content/gdrive/My Drive/Colab Notebooks/"  # Desktop file path

        try:
            self.Q = np.load(self.path + ".npy")
        except FileNotFoundError:
            self.Q = np.zeros([obs_size, self.action_space])  # Initialize Q values

    def saveQ(self, path):  # save Q values
        np.save(path + ".npy", self.Q)

    def get_Q_index(self, state):
        for i in range(len(self.pos_obs)):
            # Position
            if self.pos_obs[i] <= state[0] < self.pos_obs[i + 1]:
                # Velocity
                for j in range(len(self.velocity_obs)):
                    if self.velocity_obs[j] <= state[1] < self.velocity_obs[j + 1]:
                        return len(self.velocity_obs) * i + j  # row we need in Q

    def learn(self, episodes, until_solved, rendering):

        print("Learning MountainCar-v0 model with {} episodes ".format(episodes))
        print("Learning  model until solved status: {}\n".format(until_solved))

        global_max_score = -1e10
        global_max_height = -1e10
        episodes_to_solve = 0
        self.env.seed(0)
        scores = []
        for i in range(1, episodes):
            obs = self.env.reset()
            state = self.get_Q_index(obs)
            done = False
            total_score = 0
            max_height = -1e10
            step = 0
            while not done:
                step += 1
                self.env.render() if rendering else 0  # picture
                if random.uniform(0, 1) < self.epsilon:  # e-greedy policy
                    action = self.env.action_space.sample()
                else:
                    action = np.argmax(self.Q[state])

                next_obs, reward, done, info = self.env.step(action)
                modified_reward = reward + self.gamma * abs(next_obs[1]) - abs(obs[1])  # reward based on potentials
                next_state = self.get_Q_index(next_obs)

                # update Q
                self.Q[state, action] = (1 - self.alpha) * self.Q[state, action] + self.alpha * (
                        modified_reward + self.gamma * np.max(self.Q[next_state]) - self.Q[state, action])
                state = next_state

                total_score += reward
                max_height = max(max_height, next_obs[0])

                # end if solved
                if done and step < 200:
                    if until_solved:
                        print("Solved in {} episodes".format(i))
                        self.env.close()
                        raise SystemExit()
                    if not episodes_to_solve:
                        episodes_to_solve = i
            scores.append(total_score)
            self.epsilon -= 5 * self.epsilon / episodes if self.epsilon > 0 else 0  # epsilon reduction
            global_max_score = max(global_max_score, total_score)
            global_max_height = max(global_max_height, max_height)
            if i % 5 == 0:
                print("Episode: {}".format(i))
                print(" Total score for episode {} : {}, Max height : {}".format(i, total_score, max_height))
                print(" GLOBAL MAXIMUMS: Max score : {}, Max height  : {}".format(global_max_score, global_max_height))
                print('-' * 150)
                self.saveQ(self.path)

        print("Training finished\n")
        solve_status = "Solved in {} episodes".format(episodes_to_solve) if global_max_height >= 0.5 else "Not Solved"
        print("Max score: {} ({} : mean), Max height: {}, Solve status : {}".format(global_max_score, int(mean(scores)),
                                                                                    global_max_height,
                                                                                    solve_status))
        self.env.close()


if __name__ == '__main__':
    episode_number = 100  # number of episodes
    learn_until_solved = False  # stop if solved
    rendering = True  # picture
    epsilon = 0.1  # probability of choosing a random action
    alpha = 0.5  # learning rate
    gamma = 0.8  # discount rate
    MountainCar(epsilon, alpha, gamma).learn(episode_number, learn_until_solved, rendering)

class State:
    def __init__(self):
        self.pos = None
        self.vel = None

class Agent:
    def __init__(self, env):
        self.velocityLimit = np.array([env.observation_space.low[1], env.observation_space.high[1]])
        self.positionLimit = np.array([env.observation_space.low[0], env.observation_space.high[0]])
        self.velocityStep, self.positionStep = 0.005, 0.1
        self.velocitySpace = np.arange(self.velocityLimit[0], self.velocityLimit[1] 
                                       + self.velocityStep, self.velocityStep)
        self.positionSpace = np.arange(self.positionLimit[0], self.positionLimit[1] 
                                       + self.positionStep, self.positionStep)
        self.m, self.n, self.n_action = len(self.velocitySpace), len(self.positionSpace), 3
        self.Q = np.full(shape = (self.m, self.n, 3),
                                       fill_value = 0.0, dtype = np.float32)
        self.collectiveRecord = []
        self.success = []
        
    def getActionValueIndex(self, state):
        posOffset = state[0] - self.positionLimit[0]
        velOffset = state[1] - self.velocityLimit[0]
        posInd = posOffset // self.positionStep
        velInd = velOffset // self.velocityStep
        
        return np.array([velInd, posInd], dtype= np.int)
  
    def getAction(self, state):
        ind = self.getActionValueIndex(state, 0)
        p = self.Policy[ind[0], ind[1], :]
        action = np.random.choice([0, 1, 2], size = 1, p = p)
        return action[0]

env = wrap_env(gym.make('MountainCar-v0'))
agent = Agent(env)
env.seed(0)
print("Q Shape = ",agent.Q.shape)

eps = 0.8
changed_eps= []
changes_alpha= []
alpha = 0.2
LAMBDA = 0.8
alphaDecay = 0.999
epsDecay = 0.995
agent.e = np.zeros(shape = (agent.m, agent.n, 3))
Finish = False
numEpisodes = 5000
for i_eps in tqdm(range(1, numEpisodes + 1)):
    state = env.reset()
    agent.e[:, :, :] = 0
    gamma = 1.0
    ind = agent.getActionValueIndex(state)
    if np.random.random() < 1 - eps:
        action = np.argmax(agent.Q[ind[0], ind[1], :]) 
    else:
        action = np.random.randint(0, 3)
    
    for t in range(201):
        ind = agent.getActionValueIndex(state)
        nextState, reward, done, info = env.step(action)
        nextInd = agent.getActionValueIndex(nextState)
        
        if np.random.random() < 1 - eps:
            nextAction = np.argmax(agent.Q[nextInd[0], nextInd[1], :]) 
        else: 
            nextAction = np.random.randint(0, 3)
    
        delta = reward + gamma * agent.Q[nextInd[0],nextInd[1],nextAction] - agent.Q[ind[0],ind[1],action]
        agent.e[ind[0],ind[1],action] += 1
 
        agent.Q = np.add(agent.Q, np.multiply(alpha * delta, agent.e))
        agent.e = np.multiply(gamma * LAMBDA, agent.e)
        
        if done: 
            if t < 199:
                agent.success.append((i_eps, t))
            agent.collectiveRecord.append(-t)
            eps = max(0.0, eps * epsDecay)
            alpha = max(0.0, alpha * alphaDecay)
            break
        state = nextState
        action = nextAction
# env.close()


fig, ax = plt.subplots(figsize = (18, 8))
plt.plot(agent.collectiveRecord[:],'.')
plt.yticks(range(-110, -200, -10))
plt.ylabel("reward")
plt.xlabel("episode_number")
plt.grid()
plt.show()

avgReward = []
for i in range(100, numEpisodes):
    avgReward.append(np.mean(agent.collectiveRecord[i - 100:i]))
fig, ax = plt.subplots(figsize = (18, 8))
plt.plot(avgReward, '.')
plt.yticks(range(-110, -200, -10))
plt.xticks(changed_eps)
plt.ylabel("Avg reward for last 100 episodes")
plt.xlabel("episode_number")
plt.grid()
plt.show()

show_video()
