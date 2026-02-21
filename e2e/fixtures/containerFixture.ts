import { test as base } from '@playwright/test';
import { GenericContainer, StartedTestContainer, Wait } from 'testcontainers';
import * as path from 'path';
import { BasePage } from '../pages/BasePage';
import { ScrumPage } from '../pages/ScrumPage';
import { TaskViewPage } from '../pages/TaskViewPage';

type MyTestFixtures = {
    container: StartedTestContainer;
    basePage: BasePage;
    scrumPage: ScrumPage;
    taskViewPage: TaskViewPage;
    baseURL: string;
};

export const test = base.extend<MyTestFixtures>({
    container: [async ({ }, use) => {
        const rootPath = path.resolve(__dirname, '../../');
        const container = await new GenericContainer("lain-emacs:latest")
            .withBindMounts([
                { source: path.join(rootPath, 'sample_files', '.emacs'), target: '/root/.emacs' },
                { source: path.join(rootPath, 'sample_files', 'scrum.org'), target: '/home/agentworkstation/sources/lain-emacs/sample_files/scrum.org' },
                { source: path.join(rootPath, 'lain'), target: '/root/.emacs.d/lain' },
            ])
            .withExposedPorts(8080)
            .withCommand(["emacs", "--fg-daemon", "-l", "/root/.emacs"])
            .withWaitStrategy(Wait.forListeningPorts())
            .start();

        console.log(`Container started and port 8080 is listening.`);
        await use(container);

        console.log("Stopping container...");
        await container.stop();
    }, { scope: 'test' }],

    baseURL: async ({ container }, use) => {
        const port = container.getMappedPort(8080);
        await use(`http://127.0.0.1:${port}`);
    },

    basePage: async ({ page }, use) => {
        await use(new BasePage(page));
    },

    scrumPage: async ({ page }, use) => {
        await use(new ScrumPage(page));
    },

    taskViewPage: async ({ page }, use) => {
        await use(new TaskViewPage(page));
    },
});

export { expect } from '@playwright/test';
