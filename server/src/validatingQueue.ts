import { ChildProcessWithoutNullStreams } from 'child_process';

async function wait(ms: number) : Promise<void> {
    return new Promise<void>(resolve => {
        setTimeout(() => resolve(), ms);
    });
}

interface ValidationTask {
    key: string;
    version: number;
    callback: () => Promise<void>;
    process?: ChildProcessWithoutNullStreams;
    promise?: Promise<void>;
    resolve?: () => void;
    reject?: (error: any) => void;
    subscribers?: Array<{ resolve: () => void; reject: (error: any) => void }>;
    priority?: number; // Higher number = higher priority
}

export class ValidatingQueue {
    private runningTasks = new Map<string, ValidationTask>();
    private queuedTasks: ValidationTask[] = [];
    public maxConcurrency: number;

    constructor(maxConcurrency: number = 10) {
        this.maxConcurrency = maxConcurrency;
    }

    public get size(): number {
        return this.runningTasks.size + this.queuedTasks.length;
    }

    public get runningCount(): number {
        return this.runningTasks.size;
    }

    public get queuedCount(): number {
        return this.queuedTasks.length;
    }

    public async enqueue(key: string, version: number, callback: () => Promise<void>, priority: number = 0): Promise<void> {
        // Check if there's already a running task for this file with same version
        const runningTask = this.runningTasks.get(key);
        if (runningTask && runningTask.version === version) {
            console.log(`[queue] Task for ${key} version ${version} already running, waiting for completion`);
            return new Promise((resolve, reject) => {
                if (!runningTask.subscribers) {
                    runningTask.subscribers = [];
                }
                runningTask.subscribers.push({ resolve, reject });
            });
        }

        // Check if there's already a queued task for this file with same version
        const existingQueued = this.queuedTasks.find(task => task.key === key && task.version === version);
        if (existingQueued) {
            console.log(`[queue] Task for ${key} version ${version} already queued, waiting for completion`);
            return new Promise((resolve, reject) => {
                if (!existingQueued.subscribers) {
                    existingQueued.subscribers = [];
                }
                existingQueued.subscribers.push({ resolve, reject });
            });
        }

        // Cancel any existing tasks for this file with different version and collect subscribers
        const oldSubscribers = this.cancelTask(key);

        const task: ValidationTask = {
            key,
            version,
            callback,
            subscribers: oldSubscribers,
            priority
        };

        // Insert task in queue based on priority (higher priority first)
        let insertIndex = 0;
        while (insertIndex < this.queuedTasks.length && 
               (this.queuedTasks[insertIndex].priority || 0) >= priority) {
            insertIndex++;
        }
        this.queuedTasks.splice(insertIndex, 0, task);
        
        console.log(`[queue] Added task for ${key} (version ${version}) to queue. Queue size: ${this.queuedTasks.length}, Running: ${this.runningTasks.size}`);

        this.processQueue();
        
        // Wait for task completion
        return new Promise((resolve, reject) => {
            task.subscribers!.push({ resolve, reject });
        });
    }

    private cancelTask(key: string): Array<{ resolve: () => void; reject: (error: any) => void }> {
        const allSubscribers: Array<{ resolve: () => void; reject: (error: any) => void }> = [];
        
        // Kill running task if exists and collect subscribers
        const runningTask = this.runningTasks.get(key);
        if (runningTask) {
            console.log(`[queue] Cancelling running task for ${key}`);
            this.killProcess(runningTask);
            // Collect subscribers instead of resolving them
            if (runningTask.subscribers) {
                allSubscribers.push(...runningTask.subscribers);
            }
            this.runningTasks.delete(key);
        }

        // Remove all queued tasks for this file and collect subscribers
        const queuedIndex = this.queuedTasks.findIndex(task => task.key === key);
        if (queuedIndex !== -1) {
            const queuedTask = this.queuedTasks[queuedIndex];
            console.log(`[queue] Removing queued task for ${key}`);
            // Collect subscribers instead of resolving them
            if (queuedTask.subscribers) {
                allSubscribers.push(...queuedTask.subscribers);
            }
            this.queuedTasks.splice(queuedIndex, 1);
        }

        return allSubscribers;
    }

    private killProcess(task: ValidationTask): void {
        if (task.process && !task.process.killed) {
            console.log(`[queue] Force killing process for ${task.key} (pid: ${task.process.pid})`);
            
            // Remove all event listeners to prevent stale event handling
            task.process.removeAllListeners();
            
            // Kill immediately with SIGKILL
            const killed = task.process.kill('SIGKILL');
            console.log(`[queue] SIGKILL sent to pid ${task.process.pid}: ${killed}`);
        }
    }

    private processQueue(): void {
        // Start new tasks if there's space available
        while (this.runningTasks.size < this.maxConcurrency && this.queuedTasks.length > 0) {
            const task = this.queuedTasks.shift()!;
            this.runTask(task);
        }
    }

    private async runTask(task: ValidationTask): Promise<void> {
        this.runningTasks.set(task.key, task);
        
        console.log(`[queue] Starting task for ${task.key} (version ${task.version}). Running: ${this.runningTasks.size}/${this.maxConcurrency}`);

        try {
            await task.callback();
            // Notify all subscribers of success
            if (task.subscribers) {
                for (const subscriber of task.subscribers) {
                    subscriber.resolve();
                }
            }
        } catch (error) {
            console.error(`[queue] Task failed for ${task.key}:`, error);
            // Notify all subscribers of error
            if (task.subscribers) {
                for (const subscriber of task.subscribers) {
                    subscriber.reject(error);
                }
            }
        } finally {
            this.runningTasks.delete(task.key);
            console.log(`[queue] Task completed for ${task.key}. Running: ${this.runningTasks.size}`);
            
            // Check queue for new tasks
            this.processQueue();
        }
    }

    public setProcess(key: string, process: ChildProcessWithoutNullStreams): void {
        const task = this.runningTasks.get(key);
        if (task) {
            task.process = process;
        }
    }

    public isProcessCurrent(key: string, processId: number, version: number): boolean {
        const task = this.runningTasks.get(key);
        if (!task) {
            // No running task for this key
            return false;
        }
        if (task.version !== version) {
            // Version mismatch
            return false;
        }
        if (!task.process || task.process.pid !== processId) {
            // Process mismatch
            return false;
        }
        return true;
    }

    public async waitForFile(key: string): Promise<void> {
        // Check if there's a running task for this file
        const runningTask = this.runningTasks.get(key);
        if (runningTask) {
            return new Promise((resolve, reject) => {
                if (!runningTask.subscribers) {
                    runningTask.subscribers = [];
                }
                runningTask.subscribers.push({ resolve, reject });
            });
        }

        // Check if there's a queued task for this file
        const queuedTask = this.queuedTasks.find(task => task.key === key);
        if (queuedTask) {
            return new Promise((resolve, reject) => {
                if (!queuedTask.subscribers) {
                    queuedTask.subscribers = [];
                }
                queuedTask.subscribers.push({ resolve, reject });
            });
        }

        // No task found, return immediately
        return Promise.resolve();
    }

    public async waitAll(): Promise<void> {
        while (this.runningTasks.size > 0 || this.queuedTasks.length > 0) {
            await wait(100);
        }
    }

    public cancelAll(): void {
        // Kill all running processes and notify subscribers
        for (const task of this.runningTasks.values()) {
            this.killProcess(task);
            if (task.subscribers) {
                for (const subscriber of task.subscribers) {
                    subscriber.resolve(); // Resolve as system is shutting down
                }
            }
        }
        
        // Notify all queued task subscribers
        for (const task of this.queuedTasks) {
            if (task.subscribers) {
                for (const subscriber of task.subscribers) {
                    subscriber.resolve(); // Resolve as system is shutting down
                }
            }
        }
        
        // Clear all queues
        this.runningTasks.clear();
        this.queuedTasks = [];
        
        console.log('[queue] All tasks cancelled');
    }

    public setMaxConcurrency(maxConcurrency: number): void {
        console.log(`[queue] Setting max concurrency to ${maxConcurrency}`);
        this.maxConcurrency = maxConcurrency;
        // Process queue with new concurrency
        this.processQueue();
    }
}